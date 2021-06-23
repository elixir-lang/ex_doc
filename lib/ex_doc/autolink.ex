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

  @hexdocs "https://hexdocs.pm/"
  @otpdocs "https://erlang.org/doc/man/"

  def app_module_url(:ex_doc, module, %{current_module: module} = config) do
    ex_doc_app_url(module, config, inspect(module), config.ext, "#content")
  end

  def app_module_url(:ex_doc, module, config) do
    ex_doc_app_url(module, config, inspect(module), config.ext, "")
  end

  def app_module_url(:otp, module, _config) do
    @otpdocs <> "#{module}.html"
  end

  # TODO: make more generic
  @doc false
  def ex_doc_app_url(module, config, path, ext, suffix) do
    case :application.get_application(module) do
      {:ok, app} ->
        if app in config.apps do
          path <> ext <> suffix
        else
          config.deps
          |> Keyword.get_lazy(app, fn -> @hexdocs <> "#{app}" end)
          |> String.trim_trailing("/")
          |> Kernel.<>("/" <> path <> ".html" <> suffix)
        end

      _ ->
        path <> ext <> suffix
    end
  end
end
