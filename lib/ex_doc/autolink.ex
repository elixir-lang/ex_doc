defmodule ExDoc.Autolink do
  @moduledoc false

  # * `:apps` - the apps that the docs are being generated for. When linking modules they are
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
  # * `:extras` - map of extras
  #
  # * `:skip_undefined_reference_warnings_on` - list of modules to skip the warning on
  #
  # * `:skip_code_autolink_to` - list of terms that will be skipped when autolinking (e.g: "PrivateModule")
  #
  # * `:filtered_modules` - A list of module nodes that were filtered by the retriever

  alias ExDoc.Refs

  defstruct [
    :current_module,
    :module_id,
    :id,
    :line,
    file: "nofile",
    apps: [],
    extras: [],
    deps: [],
    ext: ".html",
    siblings: [],
    skip_undefined_reference_warnings_on: [],
    skip_code_autolink_to: [],
    filtered_modules: []
  ]

  @hexdocs "https://hexdocs.pm/"
  @otpdocs "https://www.erlang.org/doc/man/"

  def app_module_url(:ex_doc, module, %{current_module: module} = config) do
    path = module |> inspect() |> String.trim_leading(":")
    ex_doc_app_url(module, config, path, config.ext, "#content")
  end

  def app_module_url(:ex_doc, module, config) do
    path = module |> inspect() |> String.trim_leading(":")
    ex_doc_app_url(module, config, path, config.ext, "")
  end

  def app_module_url(:otp, module, _config) do
    @otpdocs <> "#{module}.html"
  end

  def app_module_url(:no_tool, _, _) do
    nil
  end

  # TODO: make more generic
  @doc false
  def ex_doc_app_url(module, config, path, ext, suffix) do
    if app = app(module) do
      if app in config.apps do
        path <> ext <> suffix
      else
        config.deps
        |> Keyword.get_lazy(app, fn -> @hexdocs <> "#{app}" end)
        |> String.trim_trailing("/")
        |> Kernel.<>("/" <> path <> ".html" <> suffix)
      end
    else
      path <> ext <> suffix
    end
  end

  defp app(module) do
    {_, app} = app_info(module)
    app
  end

  @doc false
  def tool(module, config) do
    if match?("Elixir." <> _, Atom.to_string(module)) do
      :ex_doc
    else
      {otp, app} = app_info(module)
      apps = Enum.uniq(config.apps ++ Keyword.keys(config.deps))

      if otp == true and app not in apps do
        :otp
      else
        :ex_doc
      end
    end
  end

  defp app_info(module) do
    case :code.which(module) do
      :preloaded ->
        {true, :erts}

      maybe_path ->
        otp? = is_list(maybe_path) and List.starts_with?(maybe_path, :code.lib_dir())

        app =
          case :application.get_application(module) do
            {:ok, app} ->
              app

            _ ->
              with true <- is_list(maybe_path),
                   [_, "ebin", app, "lib" | _] <- maybe_path |> Path.split() |> Enum.reverse() do
                String.to_atom(app)
              else
                _ -> nil
              end
          end

        {otp?, app}
    end
  end

  def maybe_warn(ref, config, visibility, metadata) do
    skipped = config.skip_undefined_reference_warnings_on
    file = Path.relative_to(config.file, File.cwd!())
    line = config.line

    unless Enum.any?([config.id, config.module_id, file], &(&1 in skipped)) do
      warn(ref, {file, line}, config.id, visibility, metadata)
    end
  end

  @ref_regex ~r/^`(.+)`$/

  def custom_link(attrs, config) do
    case Keyword.fetch(attrs, :href) do
      {:ok, href} ->
        case Regex.scan(@ref_regex, href) do
          [[_, custom_link]] ->
            custom_link
            |> url(:custom_link, config)
            |> remove_and_warn_if_invalid(custom_link, config)

          [] ->
            build_extra_link(href, config)
        end

      _ ->
        nil
    end
  end

  def url(string = "mix help " <> name, mode, config) do
    name |> mix_task(string, mode, config) |> maybe_remove_link(mode)
  end

  def url(string = "mix " <> name, mode, config) do
    name |> mix_task(string, mode, config) |> maybe_remove_link(mode)
  end

  def url(string, mode, config) do
    if Enum.any?(config.skip_code_autolink_to, &(&1 == string)) do
      nil
    else
      parse_url(string, mode, config)
    end
  end

  defp remove_and_warn_if_invalid(nil, reference, config) do
    warn(
      ~s[documentation references "#{reference}" but it is invalid],
      {config.file, config.line},
      config.id
    )

    :remove_link
  end

  defp remove_and_warn_if_invalid(result, _, _), do: result

  defp build_extra_link(link, config) do
    with %{scheme: nil, host: nil, path: path} = uri <- URI.parse(link),
         true <- is_binary(path) and path != "" and not (path =~ @ref_regex),
         true <- Path.extname(path) in [".livemd", ".md", ".txt", ""] do
      if file = config.extras[Path.basename(path)] do
        fragment = (uri.fragment && "#" <> uri.fragment) || ""
        file <> config.ext <> fragment
      else
        maybe_warn(nil, config, nil, %{file_path: path, original_text: link})
        nil
      end
    else
      _ -> nil
    end
  end

  defp maybe_remove_link(nil, :custom_link) do
    :remove_link
  end

  defp maybe_remove_link(result, _mode) do
    result
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

    if url in [nil, :remove_link] and mode == :custom_link do
      maybe_warn({:module, module}, config, visibility, %{
        mix_task: true,
        original_text: string
      })
    end

    url
  end

  defp module_url(module, mode, config, string) do
    ref = {:module, module}

    case {mode, Refs.get_visibility(ref)} do
      {_link_type, visibility} when visibility in [:public, :limited] ->
        app_module_url(tool(module, config), module, config)

      {:regular_link, :undefined} ->
        nil

      {:custom_link, visibility} when visibility in [:hidden, :undefined] ->
        maybe_warn(ref, config, visibility, %{original_text: string})
        :remove_link

      {_link_type, visibility} ->
        maybe_warn(ref, config, visibility, %{original_text: string})
        nil
    end
  end

  defp parse_url(string, mode, config) do
    case Regex.run(~r{^(.+)/(\d+)$}, string) do
      [_, left, right] ->
        with {:ok, arity} <- parse_arity(right) do
          {kind, rest} = kind(left)

          case parse_module_function(rest) do
            {:local, function} ->
              kind
              |> local_url(function, arity, config, string, mode: mode)
              |> maybe_remove_link(mode)

            {:remote, module, function} ->
              {kind, module, function, arity}
              |> remote_url(config, string, mode: mode)
              |> maybe_remove_link(mode)

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

      ["//", "", ""] ->
        {:local, :"..//"}

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

  # There are special forms that are forbidden by the tokenizer
  defp parse_function("__aliases__"), do: {:function, :__aliases__}
  defp parse_function("__block__"), do: {:function, :__block__}
  defp parse_function("%"), do: {:function, :%}

  defp parse_function(string) do
    case Code.string_to_quoted("& #{string}/0") do
      {:ok, {:&, _, [{:/, _, [{function, _, _}, 0]}]}} when is_atom(function) ->
        {:function, function}

      _ ->
        :error
    end
  end

  defp parse_module(<<first>> <> _ = string, _mode) when first in ?A..?Z do
    if string =~ ~r/^[A-Za-z0-9_.]+$/ do
      do_parse_module(string)
    else
      :error
    end
  end

  defp parse_module(":" <> _ = string, :custom_link) do
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

  defp kind("c:" <> rest), do: {:callback, rest}
  defp kind("t:" <> rest), do: {:type, rest}
  defp kind(rest), do: {:function, rest}

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

  def local_url(kind, name, arity, config, original_text, options \\ [])

  def local_url(:type, name, arity, config, _original_text, _options)
      when {name, arity} in @basic_types do
    ex_doc_app_url(Kernel, config, "typespecs", config.ext, "#basic-types")
  end

  def local_url(:type, name, arity, config, _original_text, _options)
      when {name, arity} in @built_in_types do
    ex_doc_app_url(Kernel, config, "typespecs", config.ext, "#built-in-types")
  end

  def local_url(kind, name, arity, config, original_text, options) do
    module = config.current_module
    ref = {kind, module, name, arity}
    mode = Keyword.get(options, :mode, :regular_link)
    visibility = Refs.get_visibility(ref)

    case {kind, visibility} do
      {_kind, :public} ->
        fragment(tool(module, config), kind, name, arity)

      {:function, _visibility} ->
        try_autoimported_function(name, arity, mode, config, original_text)

      {:type, :hidden} ->
        nil

      {:type, _} ->
        nil

      _ ->
        maybe_warn(ref, config, visibility, %{original_text: original_text})
        nil
    end
  end

  defp fragment(:ex_doc, kind, name, arity) do
    "#" <> prefix(kind) <> "#{URI.encode(Atom.to_string(name))}/#{arity}"
  end

  defp fragment(_, kind, name, arity) do
    case kind do
      :function -> "##{name}-#{arity}"
      :callback -> "#Module:#{name}-#{arity}"
      :type -> "#type-#{name}"
    end
  end

  defp prefix(kind)
  defp prefix(:function), do: ""
  defp prefix(:callback), do: "c:"
  defp prefix(:type), do: "t:"

  @autoimported_modules [Kernel, Kernel.SpecialForms]

  defp try_autoimported_function(name, arity, mode, config, original_text) do
    Enum.find_value(@autoimported_modules, fn module ->
      remote_url({:function, module, name, arity}, config, original_text,
        warn?: false,
        mode: mode
      )
    end)
  end

  def remote_url({kind, module, name, arity} = ref, config, original_text, opts \\ []) do
    warn? = Keyword.get(opts, :warn?, true)
    mode = Keyword.get(opts, :mode, :regular_link)
    same_module? = module == config.current_module

    case {mode, Refs.get_visibility({:module, module}), Refs.get_visibility(ref)} do
      {_mode, _module_visibility, :public} ->
        tool = tool(module, config)

        if same_module? do
          fragment(tool, kind, name, arity)
        else
          app_module_url(tool, module, config) <> fragment(tool, kind, name, arity)
        end

      {:regular_link, module_visibility, :undefined}
      when module_visibility == :public
      when module_visibility == :limited and kind != :type ->
        if warn?,
          do: maybe_warn(ref, config, :undefined, %{original_text: original_text})

        nil

      {:regular_link, _module_visibility, :undefined} when not same_module? ->
        nil

      {_mode, _module_visibility, visibility} ->
        if warn?,
          do: maybe_warn(ref, config, visibility, %{original_text: original_text})

        nil
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
      "documentation references #{kind} \"#{original_text}\" but it is " <>
        format_visibility(visibility, kind)

    warn(message, {file, line}, id)
  end

  defp warn(message, {file, line}, id, _, _) when is_binary(message) do
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
