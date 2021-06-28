defmodule ExDoc.Language.Erlang do
  @moduledoc false

  @behaviour ExDoc.Language

  alias ExDoc.{Autolink, Refs}

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

  @impl true
  def highlight_info() do
    %{
      language_name: "erlang",
      lexer: Makeup.Lexers.ErlangLexer,
      opts: []
    }
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
        case String.split(attrs[:href], ":") do
          [_] ->
            autolink(:module, attrs[:href], inner, config)

          [app, module] ->
            autolink_app({:module, app, module}, config)
        end

      "https://erlang.org/doc/link/seemfa" ->
        case String.split(attrs[:href], ":") do
          [_] ->
            autolink(:function, attrs[:href], inner, config)

          [app, mfa] ->
            autolink_app({:function, app, mfa}, config)
        end

      "https://erlang.org/doc/link/seetype" ->
        case String.split(attrs[:href], ":") do
          [_] ->
            autolink(:type, attrs[:href], inner, config)

          [app, mfa] ->
            autolink_app({:type, app, mfa}, config)
        end

      "https://erlang.org/doc/link/seeapp" ->
        [app, "index"] = String.split(attrs[:href], ":")
        autolink_app({:app, app}, config)

      _ ->
        ast
    end
  end

  defp walk_doc({tag, attrs, ast, meta}, config) do
    {tag, attrs, walk_doc(ast, config), meta}
  end

  defp autolink_app({:app, app}, config) do
    message = "application references are not supported: //#{app}"
    Autolink.maybe_warn(message, config, nil, %{})
    {:code, [], [app], %{}}
  end

  defp autolink_app({:module, app, module}, config) do
    message = "application references are not supported: //#{app}/#{module}"
    Autolink.maybe_warn(message, config, nil, %{})
    {:code, [], [module], %{}}
  end

  defp autolink_app({:function, app, mfa}, config) do
    mfa = String.replace(mfa, "#", ":")
    message = "application references are not supported: //#{app}/#{mfa}"
    Autolink.maybe_warn(message, config, nil, %{})
    {:code, [], [mfa], %{}}
  end

  defp autolink_app({:type, app, mfa}, config) do
    mfa = String.replace(mfa, "#", ":")
    mfa = String.trim_trailing(mfa, "/0") <> "()"
    message = "application references are not supported: //#{app}/#{mfa}"
    Autolink.maybe_warn(message, config, nil, %{})
    {:code, [], [mfa], %{}}
  end

  defp autolink(kind, string, inner, config) do
    if url = url(kind, string, config) do
      {:a, [href: url], inner, %{}}
    else
      inner
    end
  end

  defp url(:module, string, config) do
    ref = {:module, String.to_atom(string)}
    do_url(ref, string, config)
  end

  defp url(kind, string, config) do
    [module, name, arity] = String.split(string, ["#", "/"])
    name = String.to_atom(name)
    arity = String.to_integer(arity)

    original_text =
      if kind == :type do
        "#{name}()"
      else
        "#{name}/#{arity}"
      end

    if module == "" do
      ref = {kind, config.current_module, name, arity}
      visibility = Refs.get_visibility(ref)

      if visibility == :public do
        final_url({kind, name, arity}, config)
      else
        Autolink.maybe_warn(ref, config, visibility, %{original_text: original_text})
        nil
      end
    else
      ref = {kind, String.to_atom(module), name, arity}
      original_text = "#{module}:#{original_text}"
      do_url(ref, original_text, config)
    end
  end

  defp do_url(ref, original_text, config) do
    visibility = Refs.get_visibility(ref)

    if visibility == :public do
      final_url(ref, config)
    else
      Autolink.maybe_warn(ref, config, visibility, %{original_text: original_text})
      nil
    end
  end

  defp final_url({:module, module}, config) do
    tool = Autolink.tool(module)
    Autolink.app_module_url(tool, module, config)
  end

  defp final_url({kind, name, arity}, _config) do
    fragment(:ex_doc, kind, name, arity)
  end

  defp final_url({kind, module, name, arity}, config) do
    tool = Autolink.tool(module)
    module_url = Autolink.app_module_url(tool, module, config)
    module_url && module_url <> fragment(tool, kind, name, arity)
  end

  defp fragment(:otp, :function, name, arity) do
    "##{name}-#{arity}"
  end

  defp fragment(:otp, :type, name, _arity) do
    "#type-#{name}"
  end

  defp fragment(_, :function, name, arity) do
    "##{name}/#{arity}"
  end

  defp fragment(_, :type, name, arity) do
    "#t:#{name}/#{arity}"
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
end
