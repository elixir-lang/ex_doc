defmodule ExDoc.Language.Erlang do
  @moduledoc false

  @behaviour ExDoc.Language

  alias ExDoc.Autolink

  @impl true
  def module_data(module) do
    ":" <> id = inspect(module)

    %{
      id: id,
      title: id,
      type: module_type(module),
      skip: false,
      extra_callback_types: []
    }
  end

  @impl true
  def function_data(entry, module_data) do
    {{_kind, name, arity}, _anno, _signature, _doc_content, _metadata} = entry

    specs =
      case Map.fetch(module_data.specs, {name, arity}) do
        {:ok, specs} ->
          [{:attribute, 0, :spec, {{name, arity}, specs}}]

        :error ->
          []
      end

    %{
      doc_fallback: fn -> nil end,
      extra_annotations: [],
      line: nil,
      specs: specs
    }
  end

  @impl true
  def callback_data(entry, module_data) do
    {{_kind, name, arity}, _anno, _signature, _doc, _metadata} = entry

    specs =
      case Map.fetch(module_data.callbacks, {name, arity}) do
        {:ok, specs} ->
          [{:attribute, 0, :callback, {{name, arity}, specs}}]

        :error ->
          []
      end

    %{
      actual_def: {name, arity},
      line: nil,
      signature_fallback: fn -> nil end,
      specs: specs
    }
  end

  @impl true
  def type_data(_entry, spec) do
    %{
      spec: {:attribute, 0, :type, spec},
      signature_fallback: fn -> nil end
    }
  end

  @impl true
  def autolink_doc(doc, config) do
    walk(doc, config)
  end

  @impl true
  def autolink_spec(nil, _opts) do
    nil
  end

  def autolink_spec(attribute, _opts) do
    {:attribute, _, type, _} = attribute

    # `-type ` => 6
    offset = byte_size(Atom.to_string(type)) + 2

    options = [linewidth: 98 + offset]
    :erl_pp.attribute(attribute, options) |> IO.iodata_to_binary() |> trim_offset(offset)
  end

  ## Helpers

  defp module_type(module) do
    cond do
      function_exported?(module, :behaviour_info, 1) ->
        :behaviour

      true ->
        :module
    end
  end

  # `-type t() :: atom()` becomes `t() :: atom().`
  defp trim_offset(binary, offset) do
    binary
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      binary_part(line, offset, byte_size(line) - offset)
    end)
    |> Enum.join("\n")
  end

  defp walk(list, config) when is_list(list) do
    Enum.map(list, &walk(&1, config))
  end

  defp walk(binary, _) when is_binary(binary) do
    binary
  end

  defp walk({:a, attrs, inner, _meta} = ast, config) do
    case attrs[:rel] do
      "https://erlang.org/doc/link/seeerl" ->
        autolink({:module, attrs[:href]}, inner, config)

      "https://erlang.org/doc/link/seemfa" ->
        autolink({:function, attrs[:href]}, inner, config)

      "https://erlang.org/doc/link/seetype" ->
        autolink({:type, attrs[:href]}, inner, config)

      "https://erlang.org/doc/link/seeapp" ->
        [app, "index"] = String.split(attrs[:href], ":")
        maybe_warn_app_ref("//#{app}", config)
        [{:code, [], [app], %{}}]

      _ ->
        ast
    end
  end

  defp walk({tag, attrs, ast, meta}, config) do
    {tag, attrs, walk(ast, config), meta}
  end

  defp autolink({:module, string}, inner, config) do
    case String.split(string, ":") do
      [module] ->
        ref = {:module, String.to_atom(module)}
        rewrite(ref, string, inner, config)

      [app, module] ->
        maybe_warn_app_ref("//#{app}/#{module}", config)
        [{:code, [], [module], %{}}]
    end
  end

  defp autolink({kind, string}, inner, config) do
    case String.split(string, ":") do
      [rest] ->
        [module, function, arity] = String.split(rest, ["#", "/"])

        module =
          if module == "" do
            config.module
          else
            String.to_atom(module)
          end

        ref = {kind, module, String.to_atom(function), String.to_integer(arity)}
        rewrite(ref, string, inner, config)

      [app, rest] ->
        string = normalize_ref_string(rest, kind == :type)
        maybe_warn_app_ref("//#{app}/#{string}", config)
        [{:code, [], [string], %{}}]
    end
  end

  defp rewrite(ref, string, inner, config) do
    string = normalize_ref_string(string, match?({:type, _, _, _}, ref))

    case Autolink.url(ref, string, :all, config) do
      {:ok, url} ->
        [{:a, [href: url], [inner], %{}}]

      {:warn, warning} ->
        Autolink.maybe_warn(warning, config)
        inner

      :error ->
        inner
    end
  end

  defp normalize_ref_string(string, type?) do
    string =
      string
      |> String.trim_leading("#")
      |> String.replace("#", ":")

    if type? do
      String.replace_trailing(string, "/0", "()")
    else
      string
    end
  end

  defp maybe_warn_app_ref(ref, config) do
    Autolink.maybe_warn("application references are not yet supported: #{ref}", config)
  end
end
