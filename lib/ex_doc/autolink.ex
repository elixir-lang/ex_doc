defmodule ExDoc.Autolink do
  @moduledoc false

  # * `:app` - the app that the docs are being generated for. When linking modules they are
  #   checked if they are part of the app and based on that the links are relative or absolute.
  #
  # * `:current_module` - the module that the docs are being generated for. Used to link local
  #   calls and see if remote calls are in the same app.
  #
  # * `:module_id` - id of the module being documented (e.g.: `"String"`)
  #
  # * `:file` - source file location
  #
  # * `:line` - line number of the beginning of the documentation
  #
  # * `:id` - a module/function/etc being documented (e.g.: `"String.upcase/2"`)
  #
  # * `:ext` - the extension (`".html"`, "`.xhtml"`, etc)
  #
  # * `:siblings` - applications in the same umbrella project as `:app`. When linking modules,
  #   links to these applications are relative.
  #
  # * `:extras` - list of extras
  #
  # * `:skip_undefined_reference_warnings_on` - list of modules to skip the warning on

  @enforce_keys [:app, :file]

  defstruct [
    :app,
    :current_module,
    :module_id,
    :id,
    :file,
    :line,
    extras: [],
    ext: ".html",
    siblings: [],
    skip_undefined_reference_warnings_on: []
  ]

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates, as: T
  alias ExDoc.Refs

  @hexdocs "https://hexdocs.pm/"
  @otpdocs "http://www.erlang.org/doc/man/"

  @autoimported_modules [Kernel, Kernel.SpecialForms]

  def doc(ast, options \\ []) do
    config = struct!(__MODULE__, options)
    walk(ast, config)
  end

  defp walk(list, config) when is_list(list) do
    Enum.map(list, &walk(&1, config))
  end

  defp walk(binary, _) when is_binary(binary) do
    binary
  end

  defp walk({:pre, _, _} = ast, _config) do
    ast
  end

  defp walk({:a, attrs, inner} = ast, config) do
    cond do
      url = custom_link(attrs, config) ->
        {:a, Keyword.put(attrs, :href, url), inner}

      true ->
        ast
    end
  end

  defp walk({:code, attrs, [code]} = ast, config) do
    if url = url(code, :regular_link, config) do
      code = remove_prefix(code)
      {:a, [href: url], [{:code, attrs, [code]}]}
    else
      ast
    end
  end

  defp walk({tag, attrs, ast}, config) do
    {tag, attrs, walk(ast, config)}
  end

  defp custom_link(attrs, config) do
    case Keyword.fetch(attrs, :href) do
      {:ok, href} ->
        case Regex.scan(~r/^`(.+)`$/, href) do
          [[_, custom_link]] ->
            build_custom_link(custom_link, config)

          [] ->
            build_extra_link(href, config)
        end

      _ ->
        nil
    end
  end

  defp build_custom_link(link, config) do
    url(link, :custom_link, config)
  end

  defp build_extra_link(link, config) do
    with uri <- URI.parse(link),
         nil <- uri.host,
         true <- is_binary(uri.path),
         extension when extension in [".md", ".txt", ""] <- Path.extname(uri.path) do
      file = Path.basename(uri.path)

      if file in config.extras do
        without_ext = trim_extension(file, extension)
        fragment = (uri.fragment && "#" <> uri.fragment) || ""
        HTML.text_to_id(without_ext) <> config.ext <> fragment
      else
        message = "documentation references file \"#{uri.path}\" but it doesn't exist"
        maybe_warn(message, config, :undefined)

        nil
      end
    else
      _ -> nil
    end
  end

  defp trim_extension(file, ""),
    do: file

  defp trim_extension(file, extension),
    do: String.trim_trailing(file, extension)

  @basic_types [
    any: 0,
    none: 0,
    atom: 0,
    map: 0,
    pid: 0,
    port: 0,
    reference: 0,
    struct: 0,
    tuple: 0,
    integer: 0,
    float: 0,
    neg_integer: 0,
    non_neg_integer: 0,
    pos_integer: 0,
    list: 1,
    nonempty_list: 1,
    improper_list: 2,
    maybe_improper_list: 2
  ]

  @built_in_types [
    term: 0,
    arity: 0,
    as_boolean: 1,
    binary: 0,
    bitstring: 0,
    boolean: 0,
    byte: 0,
    char: 0,
    charlist: 0,
    nonempty_charlist: 0,
    fun: 0,
    function: 0,
    identifier: 0,
    iodata: 0,
    iolist: 0,
    keyword: 0,
    keyword: 1,
    list: 0,
    nonempty_list: 0,
    maybe_improper_list: 0,
    nonempty_maybe_improper_list: 0,
    mfa: 0,
    module: 0,
    no_return: 0,
    node: 0,
    number: 0,
    struct: 0,
    timeout: 0
  ]

  defp url("", _mode, _config), do: nil
  defp url("mix help " <> name, mode, config), do: mix_task(name, mode, config)
  defp url("mix " <> name, mode, config), do: mix_task(name, mode, config)

  defp url(code, mode, config) do
    case String.split(code, "/") do
      [left, right] ->
        with {:ok, arity} <- parse_arity(right) do
          {kind, rest} = kind(left)

          case parse_module_function(rest) do
            {:local, function} ->
              local_url(kind, function, arity, config)

            {:remote, module, function} ->
              remote_url(kind, module, function, arity, config)

            :error ->
              nil
          end
        else
          _ ->
            nil
        end

      [string] ->
        case parse_module(string, mode) do
          {:module, module} ->
            module_url(module, string, mode, config)

          :error ->
            nil
        end

      _ ->
        nil
    end
  end

  defp kind("c:" <> rest), do: {:callback, rest}
  defp kind("t:" <> rest), do: {:type, rest}
  defp kind(rest), do: {:function, rest}

  defp remove_prefix("c:" <> rest), do: rest
  defp remove_prefix("t:" <> rest), do: rest
  defp remove_prefix(rest), do: rest

  defp parse_arity(string) do
    case Integer.parse(string) do
      {arity, ""} -> {:ok, arity}
      _ -> :error
    end
  end

  defp parse_module_function(string) do
    case string |> String.split(".") |> Enum.reverse() do
      [string] ->
        with {:function, function} <- parse_function(string) do
          {:local, function}
        end

      ["", "", ""] ->
        {:local, :..}

      ["", ""] ->
        {:local, :.}

      ["", "", "" | rest] ->
        module_string = rest |> Enum.reverse() |> Enum.join(".")

        with {:module, module} <- parse_module(module_string, :custom_link) do
          {:remote, module, :..}
        end

      ["", "" | rest] ->
        module_string = rest |> Enum.reverse() |> Enum.join(".")

        with {:module, module} <- parse_module(module_string, :custom_link) do
          {:remote, module, :.}
        end

      [function_string | rest] ->
        module_string = rest |> Enum.reverse() |> Enum.join(".")

        with {:module, module} <- parse_module(module_string, :custom_link),
             {:function, function} <- parse_function(function_string) do
          {:remote, module, function}
        end
    end
  end

  defp parse_module(<<first>> <> _ = string, _mode) when first in ?A..?Z do
    do_parse_module(string)
  end

  defp parse_module(<<?:>> <> _ = string, :custom_link) do
    do_parse_module(string)
  end

  defp parse_module(_, _) do
    :error
  end

  defp do_parse_module(string) do
    case Code.string_to_quoted(string, warn_on_unnecessary_quotes: false) do
      {:ok, module} when is_atom(module) -> {:module, module}
      {:ok, {:__aliases__, _, parts}} -> {:module, Module.concat(parts)}
      _ -> :error
    end
  end

  defp parse_function(string) do
    case Code.string_to_quoted(":" <> string) do
      {:ok, function} when is_atom(function) -> {:function, function}
      _ -> :error
    end
  end

  defp mix_task(name, mode, config) do
    if name =~ ~r/^[a-z][a-z0-9]*(\.[a-z][a-z0-9]*)*$/ do
      parts = name |> String.split(".") |> Enum.map(&Macro.camelize/1)
      module_url(Module.concat([Mix, Tasks | parts]), name, mode, config, true)
    end
  end

  @doc """
  Converts given types/specs `ast` into HTML with links.
  """
  def typespec(ast, options) do
    config = struct!(__MODULE__, options)

    string =
      ast
      |> Macro.to_string()
      |> Code.format_string!(line_length: 80)
      |> IO.iodata_to_binary()

    name = typespec_name(ast)
    {name, rest} = split_name(string, name)
    name <> do_typespec(rest, config)
  end

  defp typespec_name({:"::", _, [{name, _, _}, _]}), do: Atom.to_string(name)
  defp typespec_name({:when, _, [left, _]}), do: typespec_name(left)
  defp typespec_name({name, _, _}) when is_atom(name), do: Atom.to_string(name)

  # extract out function name so we don't process it. This is to avoid linking it when there's
  # a type with the same name
  defp split_name(string, name) do
    if String.starts_with?(string, name) do
      {name, binary_part(string, byte_size(name), byte_size(string) - byte_size(name))}
    else
      {"", string}
    end
  end

  defp do_typespec(string, config) do
    regex =
      ~r/((?:((?:\:[a-z][_a-zA-Z0-9]*)|(?:[A-Z][_a-zA-Z0-9]*(?:\.[A-Z][_a-zA-Z0-9]*)*))\.)?(\w+!?))(\(.*\))/

    Regex.replace(regex, string, fn _all, call_string, module_string, name_string, rest ->
      module = string_to_module(module_string)
      name = String.to_atom(name_string)
      arity = count_args(rest, 0, 0)

      url =
        if module do
          remote_url(:type, module, name, arity, config)
        else
          local_url(:type, name, arity, config)
        end

      if url do
        ~s[<a href="#{url}">#{T.h(call_string)}</a>]
      else
        call_string
      end <> do_typespec(rest, config)
    end)
  end

  defp string_to_module(""), do: nil

  defp string_to_module(string) do
    if String.starts_with?(string, ":") do
      string |> String.trim_leading(":") |> String.to_atom()
    else
      Module.concat([string])
    end
  end

  defp count_args("()" <> _, 0, 0), do: 0
  defp count_args("(" <> rest, counter, acc), do: count_args(rest, counter + 1, acc)
  defp count_args("[" <> rest, counter, acc), do: count_args(rest, counter + 1, acc)
  defp count_args("{" <> rest, counter, acc), do: count_args(rest, counter + 1, acc)
  defp count_args(")" <> _, 1, acc), do: acc + 1
  defp count_args(")" <> rest, counter, acc), do: count_args(rest, counter - 1, acc)
  defp count_args("]" <> rest, counter, acc), do: count_args(rest, counter - 1, acc)
  defp count_args("}" <> rest, counter, acc), do: count_args(rest, counter - 1, acc)
  defp count_args("," <> rest, 1, acc), do: count_args(rest, 1, acc + 1)
  defp count_args(<<_>> <> rest, counter, acc), do: count_args(rest, counter, acc)
  defp count_args("", _counter, acc), do: acc

  ## Internals

  defp module_url(module, module_string, mode, config, mix_task? \\ false)

  defp module_url(module, _module_string, _mode, %{current_module: module}, _mix_task?),
    do: "#content"

  defp module_url(module, module_string, mode, config, mix_task?) do
    case {mode, Refs.get_visibility({:module, module})} do
      {_, :public} ->
        module_url_app(tool(module), module, config)

      {:regular_link, :undefined} ->
        nil

      {_, visibility} ->
        module_string =
          if mix_task? do
            "mix " <> module_string
          else
            module_string
          end

        message = "documentation references module \"#{module_string}\" but it is #{visibility}"

        maybe_warn(message, config, visibility)

        nil
    end
  end

  defp module_url_app(:ex_doc, module, config) do
    ex_doc_app_url(module, config) <> inspect(module) <> config.ext
  end

  defp module_url_app(:otp, module, _config) do
    @otpdocs <> "#{module}.html"
  end

  defp local_url(:type, name, arity, config) when {name, arity} in @basic_types do
    ex_doc_app_url(Kernel, config) <> "typespecs" <> config.ext <> "#basic-types"
  end

  defp local_url(:type, name, arity, config) when {name, arity} in @built_in_types do
    ex_doc_app_url(Kernel, config) <> "typespecs" <> config.ext <> "#built-in-types"
  end

  defp local_url(kind, name, arity, config) do
    module = config.current_module
    ref = {kind, module, name, arity}

    cond do
      Refs.public?(ref) -> fragment(tool(module), kind, name, arity)
      kind == :function -> try_autoimported_function(name, arity, config)
      true -> nil
    end
  end

  defp try_autoimported_function(name, arity, config) do
    Enum.find_value(@autoimported_modules, fn module ->
      remote_url(:function, module, name, arity, config, warn?: false)
    end)
  end

  defp remote_url(kind, module, name, arity, config, opts \\ []) do
    warn? = Keyword.get(opts, :warn?, true)

    case Refs.get_visibility({kind, module, name, arity}) do
      :public ->
        case tool(module) do
          :no_tool ->
            nil

          tool ->
            if module == config.current_module do
              fragment(tool, kind, name, arity)
            else
              module_url_app(tool, module, config) <> fragment(tool, kind, name, arity)
            end
        end

      visibility ->
        if warn? do
          maybe_warn({kind, module, name, arity}, config, visibility)
        end

        nil
    end
  end

  defp ex_doc_app_url(module, config) do
    app = config.app

    case :application.get_application(module) do
      {:ok, ^app} -> ""
      {:ok, app} -> if app in config.siblings, do: "", else: @hexdocs <> "#{app}/"
      _ -> ""
    end
  end

  defp fragment(:ex_doc, kind, name, arity) do
    prefix =
      case kind do
        :function -> ""
        :callback -> "c:"
        :type -> "t:"
      end

    "#" <> prefix <> "#{T.enc(Atom.to_string(name))}/#{arity}"
  end

  defp fragment(:otp, kind, name, arity) do
    case kind do
      :function -> "##{name}-#{arity}"
      :callback -> "#Module:#{name}-#{arity}"
      :type -> "#type-#{name}"
    end
  end

  defp tool(module) do
    name = Atom.to_string(module)

    if name == String.downcase(name) do
      case :code.which(module) do
        :preloaded ->
          :otp

        :non_existing ->
          :no_tool

        path ->
          if String.starts_with?(List.to_string(path), List.to_string(:code.lib_dir())) do
            :otp
          else
            :no_tool
          end
      end
    else
      :ex_doc
    end
  end

  defp maybe_warn(kmfa_or_message, config, visibility) do
    skipped = config.skip_undefined_reference_warnings_on
    file = Path.relative_to(config.file, File.cwd!())
    line = config.line
    id = config.id

    unless Enum.any?([id, config.module_id, file], &(&1 in skipped)) do
      case kmfa_or_message do
        {kind, module, name, arity} ->
          warn({kind, module, name, arity}, {file, line}, id, visibility)

        message when is_binary(message) ->
          warn(message, {file, line}, id, visibility)
      end
    end
  end

  defp warn({kind, module, name, arity}, {file, line}, id, visibility) do
    message =
      "documentation references #{kind} \"#{inspect(module)}.#{name}/#{arity}\"" <>
        " but it is #{visibility}"

    warn(message, {file, line}, id, visibility)
  end

  defp warn(message, {file, line}, id, _visibility) do
    warning = IO.ANSI.format([:yellow, "warning: ", :reset])

    stacktrace =
      "  #{file}" <>
        if(line, do: ":#{line}", else: "") <>
        if(id, do: ": #{id}", else: "")

    IO.puts(:stderr, [warning, message, ?\n, stacktrace, ?\n])
  end
end
