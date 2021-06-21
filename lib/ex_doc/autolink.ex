defmodule ExDoc.Autolink do
  @moduledoc false

  alias ExDoc.Formatter.HTML.Templates, as: T

  # * `:apps` - the apps that the docs are being generated for. When linking modules they are
  #   checked if they are part of the app and based on that the links are relative or absolute.
  #
  # * `:module` - the module that the docs are being generated for. Used to link local
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

  defstruct apps: [],
            deps: [],
            siblings: [],
            module: nil,
            module_id: nil,
            file: nil,
            line: nil,
            id: nil,
            ext: ".html",
            extras: [],
            skip_undefined_reference_warnings_on: []

  @doc """
  Returns url for the given ref.

  `warn` controls whether warning is emitted for missing refs. It can be one of:

    * `:all` - all missing refs

    * `:public_module` - when `{_kind, module, fun, arity}` is missing, but `module`
      is public

    * `:none` - don't emit warning

  """
  def url(ref, code, warn, config) do
    case ref do
      {_kind, module, _function, _arity} ->
        Code.ensure_loaded(module)

        case do_url(ref, code, warn, config) do
          {:ok, url} ->
            {:ok, url}

          {:warn, warning} ->
            {:warn, warning}

          :error ->
            if warn in [:all, :public_module] and
                 ExDoc.Refs.get_visibility({:module, module}) == :public do
              {:warn, {:ref, code, :undefined}}
            else
              :error
            end
        end

      {kind, function, arity} ->
        ref = {kind, config.module, function, arity}
        do_url(ref, code, warn, config)

      ref ->
        do_url(ref, code, warn, config)
    end
  end

  @doc """
  Emits the warning unless it should be skipped per config.
  """
  def maybe_warn(warning, config) do
    skipped = config.skip_undefined_reference_warnings_on
    id = config.id
    file = (config.file && Path.relative_to(config.file, File.cwd!())) || "nofile"
    line = config.line

    unless Enum.any?([config.id, config.module_id, file], &(&1 in skipped)) do
      stacktrace =
        "  #{file}" <>
          if(line, do: ":#{line}", else: "") <>
          if(id, do: ": #{id}", else: "")

      message =
        case warning do
          message when is_binary(message) ->
            message

          {:ref, string, visibility} ->
            "documentation references \"#{string}\" but it is #{visibility}"

          {:extras, path} ->
            "documentation references file \"#{path}\" but it does not exists"
        end

      IO.puts(
        :stderr,
        IO.ANSI.format([:yellow, "warning: ", :reset, message, ?\n, stacktrace, ?\n])
      )
    end
  end

  defp do_url(ref, code, warn, config) do
    visibility = ExDoc.Refs.get_visibility(ref)
    module = config.module

    cond do
      visibility == :public ->
        {:ok, url(ref, config)}

      # allow local refs to typeps without warnings
      visibility == :hidden and match?({:type, ^module, _, _}, ref) ->
        :error

      visibility == :hidden ->
        {:warn, {:ref, code, visibility}}

      warn == :all ->
        {:warn, {:ref, code, visibility}}

      true ->
        :error
    end
  end

  # Generates url for the given ref. The ref is valid by this point.
  defp url(ref, config)

  defp url({:module, module}, config) do
    if module == config.module do
      "#content"
    else
      app = app(module)
      doc_tool = doc_tool(app, module, config)
      base_url(doc_tool, app, module, config)
    end
  end

  defp url({kind, module, function, arity}, config) do
    app = app(module)
    doc_tool = doc_tool(app, module, config)

    base_url =
      if module == config.module do
        ""
      else
        base_url(doc_tool, app, module, config)
      end

    base_url <> function_url(doc_tool, kind, function, arity)
  end

  @doc false
  def base_url(doc_tool, app, module_or_extras, config) do
    cond do
      app in config.apps ->
        page_url(module_or_extras, config.ext)

      app && doc_tool == :ex_doc ->
        if app in config.siblings do
          page_url(module_or_extras, config.ext)
        else
          Keyword.get_lazy(config.deps, app, fn ->
            "https://hexdocs.pm/#{app}/"
          end) <> page_url(module_or_extras, ".html")
        end

      doc_tool == :otp ->
        "https://erlang.org/doc/man/" <> page_url(module_or_extras, ".html")

      true ->
        page_url(module_or_extras, config.ext)
    end
  end

  defp page_url(nil, _ext) do
    ""
  end

  defp page_url(module, ext) when is_atom(module) do
    String.trim_leading(inspect(module), ":") <> ext
  end

  defp page_url(extras, ext) when is_binary(extras) do
    extras <> ext
  end

  defp function_url(:ex_doc, kind, name, arity) do
    prefix =
      case kind do
        :function -> ""
        :callback -> "c:"
        :type -> "t:"
      end

    name = name |> Atom.to_string() |> T.enc()
    "#" <> prefix <> "#{name}/#{arity}"
  end

  defp function_url(:otp, :function, name, arity) do
    "##{name}-#{arity}"
  end

  defp function_url(:otp, :callback, name, arity) do
    "#Module:#{name}-#{arity}"
  end

  defp function_url(:otp, :type, name, _arity) do
    "#type-#{name}"
  end

  ## Helpers

  defp doc_tool(app, module, config) do
    cond do
      app in config.apps ->
        :ex_doc

      module |> Atom.to_string() |> String.starts_with?("Elixir.") ->
        :ex_doc

      true ->
        which = :code.which(module)

        cond do
          which == :preloaded ->
            :otp

          is_list(which) and :lists.prefix(:code.lib_dir(), which) ->
            :otp

          true ->
            :ex_doc
        end
    end
  end

  defp app(module) do
    case :application.get_application(module) do
      {:ok, app} ->
        app

      :undefined ->
        case :code.is_loaded(module) do
          {:file, :preloaded} ->
            :erts

          _ ->
            nil
        end
    end
  end
end
