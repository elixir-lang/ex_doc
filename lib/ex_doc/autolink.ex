defmodule ExDoc.Autolink do
  @moduledoc false

  # * `:apps` - the app that the docs are being generated for. When linking modules they are
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
  # * `:extras` - list of extras
  #
  # * `:skip_undefined_reference_warnings_on` - list of modules to skip the warning on

  @enforce_keys [:file]
  defstruct [
    :current_module,
    :module_id,
    :id,
    :file,
    :line,
    apps: [],
    extras: [],
    deps: [],
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

  @ref_regex ~r/^`(.+)`$/

  defp custom_link(attrs, config) do
    case Keyword.fetch(attrs, :href) do
      {:ok, href} ->
        case Regex.scan(@ref_regex, href) do
          [[_, custom_link]] ->
            url(custom_link, :custom_link, config)

          [] ->
            build_extra_link(href, config)
        end

      _ ->
        nil
    end
  end

  defp build_extra_link(link, config) do
    with uri <- URI.parse(link),
         nil <- uri.host,
         true <- is_binary(uri.path),
         false <- uri.path =~ @ref_regex,
         extension when extension in [".md", ".txt", ""] <- Path.extname(uri.path) do
      file = Path.basename(uri.path)

      if file in config.extras do
        without_ext = trim_extension(file, extension)
        fragment = (uri.fragment && "#" <> uri.fragment) || ""
        HTML.text_to_id(without_ext) <> config.ext <> fragment
      else
        maybe_warn(nil, config, nil, %{file_path: uri.path, original_text: link})

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
    float: 0,
    integer: 0,
    neg_integer: 0,
    non_neg_integer: 0,
    pos_integer: 0,
    list: 1,
    nonempty_list: 1,
    maybe_improper_list: 2,
    nonempty_improper_list: 2,
    nonempty_maybe_improper_list: 2
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

  defp url(string = "mix help " <> name, mode, config), do: mix_task(name, string, mode, config)
  defp url(string = "mix " <> name, mode, config), do: mix_task(name, string, mode, config)

  defp url(string, mode, config) do
    case Regex.run(~r{^(.+)/(\d+)$}, string) do
      [_, left, right] ->
        with {:ok, arity} <- parse_arity(right) do
          {kind, rest} = kind(left)

          case parse_module_function(rest) do
            {:local, function} ->
              local_url(kind, function, arity, config, string, mode: mode)

            {:remote, module, function} ->
              remote_url({kind, module, function, arity}, config, string, mode: mode)

            :error ->
              nil
          end
        else
          _ ->
            nil
        end

      nil ->
        case parse_module(string, mode) do
          {:module, module} ->
            module_url(module, mode, config, string)

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
      {:ok, module} when is_atom(module) ->
        {:module, module}

      {:ok, {:__aliases__, _, parts}} ->
        if Enum.all?(parts, &is_atom/1) do
          {:module, Module.concat(parts)}
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp parse_function(string) do
    case Code.string_to_quoted(":" <> string) do
      {:ok, function} when is_atom(function) -> {:function, function}
      _ -> :error
    end
  end

  defp mix_task(name, string, mode, config) do
    {module, url, visibility} =
      if name =~ ~r/^[a-z][a-z0-9]*(\.[a-z][a-z0-9]*)*$/ do
        parts = name |> String.split(".") |> Enum.map(&Macro.camelize/1)
        module = Module.concat([Mix, Tasks | parts])

        {module, module_url(module, :regular_link, config, string),
         Refs.get_visibility({:module, module})}
      else
        {nil, nil, :undefined}
      end

    if is_nil(url) and mode == :custom_link do
      maybe_warn({:module, module}, config, visibility, %{mix_task: true, original_text: string})
    end

    url
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
      |> T.h()

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
    regex = ~r{
        (                                             # <call_string>
          (?:
            (                                         # <module_string>
              (?:
                \:[a-z][_a-zA-Z0-9]*                  # Erlang module
              )|
              (?:
                [A-Z][_a-zA-Z0-9]*                    # Elixir module
                (?:\.[A-Z][_a-zA-Z0-9]*)*             # Elixir submodule
              )
            )                                         # </module_string>
            \.                                        # Dot operator
          )?
          ([a-z_][_a-zA-Z0-9]*[\?\!]?)                # Name <name_string />
        )                                             # </call_string>
        (\(.*\))                                      # Arguments <rest />
      }x

    Regex.replace(regex, string, fn _all, call_string, module_string, name_string, rest ->
      module = string_to_module(module_string)
      name = String.to_atom(name_string)
      arity = count_args(rest, 0, 0)

      url =
        if module do
          remote_url({:type, module, name, arity}, config, string)
        else
          local_url(:type, name, arity, config, string)
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

  defp module_url(module, _mode, %{current_module: module}, _string) do
    "#content"
  end

  defp module_url(module, mode, config, string) do
    ref = {:module, module}

    case {mode, Refs.get_visibility(ref)} do
      {_, :public} ->
        app_module_url(tool(module), module, config)

      {:regular_link, :undefined} ->
        nil

      {_, visibility} ->
        maybe_warn(ref, config, visibility, %{original_text: string})

        nil
    end
  end

  defp app_module_url(:ex_doc, module, config) do
    ex_doc_app_url(module, config) <> inspect(module) <> config.ext
  end

  defp app_module_url(:otp, module, _config) do
    @otpdocs <> "#{module}.html"
  end

  defp local_url(kind, name, arity, config, original_text, options \\ [])

  defp local_url(:type, name, arity, config, _original_text, _options)
       when {name, arity} in @basic_types do
    ex_doc_app_url(Kernel, config) <> "typespecs" <> config.ext <> "#basic-types"
  end

  defp local_url(:type, name, arity, config, _original_text, _options)
       when {name, arity} in @built_in_types do
    ex_doc_app_url(Kernel, config) <> "typespecs" <> config.ext <> "#built-in-types"
  end

  defp local_url(kind, name, arity, config, original_text, options) do
    module = config.current_module
    ref = {kind, module, name, arity}
    mode = Keyword.get(options, :mode, :regular_link)
    visibility = Refs.get_visibility(ref)

    case {kind, visibility} do
      {_kind, :public} ->
        fragment(tool(module), kind, name, arity)

      {:function, _visibility} ->
        try_autoimported_function(name, arity, mode, config, original_text)

      {:type, :hidden} ->
        nil

      # skip `@type %{required(...), optional(...), ...}`
      {:type, _visibility} when name in [:required, :optional] and arity == 1 ->
        nil

      _ ->
        maybe_warn(ref, config, visibility, %{original_text: original_text})
        nil
    end
  end

  defp try_autoimported_function(name, arity, mode, config, original_text) do
    Enum.find_value(@autoimported_modules, fn module ->
      remote_url({:function, module, name, arity}, config, original_text, warn?: false, mode: mode)
    end)
  end

  defp remote_url({kind, module, name, arity} = ref, config, original_text, opts \\ []) do
    warn? = Keyword.get(opts, :warn?, true)
    mode = Keyword.get(opts, :mode, :regular_link)
    same_module? = module == config.current_module

    case {mode, Refs.get_visibility({:module, module}), Refs.get_visibility(ref)} do
      {_mode, _module_visibility, :public} ->
        case tool(module) do
          :no_tool ->
            nil

          tool ->
            if same_module? do
              fragment(tool, kind, name, arity)
            else
              app_module_url(tool, module, config) <> fragment(tool, kind, name, arity)
            end
        end

      {:regular_link, :public, :undefined} ->
        if warn?, do: maybe_warn(ref, config, :undefined, %{original_text: original_text})
        nil

      {:regular_link, _module_visibility, :undefined} when not same_module? ->
        nil

      {_mode, _module_visibility, visibility} ->
        if warn?, do: maybe_warn(ref, config, visibility, %{original_text: original_text})

        nil
    end
  end

  defp ex_doc_app_url(module, config) do
    case :application.get_application(module) do
      {:ok, app} ->
        if app in config.apps do
          ""
        else
          Keyword.get_lazy(config.deps, app, fn -> @hexdocs <> "#{app}/" end)
        end

      _ ->
        ""
    end
  end

  defp prefix(kind)
  defp prefix(:function), do: ""
  defp prefix(:callback), do: "c:"
  defp prefix(:type), do: "t:"

  defp fragment(:ex_doc, kind, name, arity) do
    "#" <> prefix(kind) <> "#{T.enc(Atom.to_string(name))}/#{arity}"
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

  defp maybe_warn(ref, config, visibility, metadata) do
    skipped = config.skip_undefined_reference_warnings_on
    file = Path.relative_to(config.file, File.cwd!())
    line = config.line

    unless Enum.any?([config.id, config.module_id, file], &(&1 in skipped)) do
      warn(ref, {file, line}, config.id, visibility, metadata)
    end
  end

  defp warn(message, {file, line}, id) do
    warning = IO.ANSI.format([:yellow, "warning: ", :reset])

    stacktrace =
      "  #{file}" <>
        if(line, do: ":#{line}", else: "") <>
        if(id, do: ": #{id}", else: "")

    IO.puts(:stderr, [warning, message, ?\n, stacktrace, ?\n])
  end

  defp warn(ref, file_line, id, visibility, metadata)

  defp warn(
         {:module, _module},
         {file, line},
         id,
         visibility,
         %{mix_task: true, original_text: original_text}
       ) do
    message =
      "documentation references \"#{original_text}\" but it is " <>
        format_visibility(visibility, :module)

    warn(message, {file, line}, id)
  end

  defp warn(
         {:module, _module},
         {file, line},
         id,
         visibility,
         %{original_text: original_text}
       ) do
    message =
      "documentation references module \"#{original_text}\" but it is " <>
        format_visibility(visibility, :module)

    warn(message, {file, line}, id)
  end

  defp warn(
         nil,
         {file, line},
         id,
         _visibility,
         %{file_path: _file_path, original_text: original_text}
       ) do
    message = "documentation references file \"#{original_text}\" but it does not exist"

    warn(message, {file, line}, id)
  end

  defp warn(
         {kind, _module, _name, _arity},
         {file, line},
         id,
         visibility,
         %{original_text: original_text}
       ) do
    message =
      "documentation references \"#{original_text}\" but it is " <>
        format_visibility(visibility, kind)

    warn(message, {file, line}, id)
  end

  # there is not such a thing as private callback or private module
  defp format_visibility(visibility, kind) when kind in [:module, :callback], do: "#{visibility}"

  # typep is defined as :hidden, since there is no :private visibility value
  # but type defined with @doc false also is the stored the same way.
  defp format_visibility(:hidden, :type), do: "hidden or private"

  # for the rest, it can either be undefined or private
  defp format_visibility(:undefined, _kind), do: "undefined or private"
  defp format_visibility(visibility, _kind), do: "#{visibility}"
end
