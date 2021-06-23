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
  def autolink_doc(ast, opts) do
    config = struct!(Autolink, opts)
    walk_doc(ast, config)
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

  ## Autolink

  defp walk_doc(list, config) when is_list(list) do
    Enum.map(list, &walk_doc(&1, config))
  end

  defp walk_doc(binary, _) when is_binary(binary) do
    binary
  end

  defp walk_doc({:a, attrs, inner, _meta} = ast, config) do
    case attrs[:rel] do
      "https://erlang.org/doc/link/seeerl" ->
        autolink({:module, attrs[:href]}, inner, config)

      "https://erlang.org/doc/link/seemfa" ->
        autolink({:function, attrs[:href]}, inner, config)

      "https://erlang.org/doc/link/seetype" ->
        autolink({:type, attrs[:href]}, inner, config)

      _ ->
        ast
    end
  end

  defp walk_doc({tag, attrs, ast, meta}, config) do
    {tag, attrs, walk_doc(ast, config), meta}
  end

  defp autolink(ref, inner, config) do
    if url = url(ref, config) do
      {:a, [href: url], inner, %{}}
    else
      inner
    end
  end

  defp url({:module, string}, config) do
    module = String.to_atom(string)

    if otp_module?(module) do
      Autolink.app_module_url(:otp, module, config)
    else
      "#{module}#{config.ext}"
    end
  end

  defp url({kind, string}, config) do
    [module, name, arity] = String.split(string, ["#", "/"])

    if module == "" do
      case kind do
        :function ->
          "##{name}/#{arity}"

        :type ->
          "#t:#{name}/#{arity}"
      end
    else
      module = String.to_atom(module)

      fragment =
        case kind do
          :function ->
            "##{name}-#{arity}"

          :type ->
            "#type-#{name}"
        end

      if otp_module?(module) do
        Autolink.app_module_url(:otp, module, config) <> fragment
      else
        "#{module}#{config.ext}" <> fragment
      end
    end
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

  defp otp_module?(module) do
    case :code.which(module) do
      :preloaded ->
        true

      path ->
        :string.prefix(path, :code.lib_dir()) != :nomatch
    end
  end
end
