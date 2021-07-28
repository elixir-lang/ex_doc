defmodule ExDoc.Language.Erlang do
  @moduledoc false

  @behaviour ExDoc.Language

  alias ExDoc.{Autolink, Refs}

  @impl true
  def module_data(_module, {:docs_v1, _, _, _, doc, _, _}, _config) when not is_map(doc) do
    :skip
  end

  def module_data(module, docs_chunk, _config) do
    ":" <> id = inspect(module)
    abst_code = get_abstract_code(module)
    line = find_module_line(module, abst_code)

    %{
      module: module,
      docs: docs_chunk,
      language: __MODULE__,
      id: id,
      title: id,
      type: module_type(module),
      line: line,
      callback_types: [:callback],
      nesting_info: nil,
      private: %{
        abst_code: abst_code,
        specs: get_specs(module),
        callbacks: get_callbacks(module)
      }
    }
  end

  @impl true
  def function_data(entry, module_data) do
    {{_kind, name, arity}, _anno, _signature, _doc_content, _metadata} = entry

    specs =
      case Map.fetch(module_data.private.specs, {name, arity}) do
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
    {{_kind, name, arity}, anno, signature, _doc, _metadata} = entry

    specs =
      case Map.fetch(module_data.private.callbacks, {name, arity}) do
        {:ok, specs} ->
          [{:attribute, 0, :callback, {{name, arity}, specs}}]

        :error ->
          []
      end

    %{
      actual_def: {name, arity},
      line: anno_line(anno),
      signature: signature,
      specs: specs
    }
  end

  @impl true
  def type_data(entry, module_data) do
    {{_kind, name, arity}, _anno, signature, _doc, _metadata} = entry

    %{type: type, spec: spec, line: line} =
      ExDoc.Language.Elixir.type_from_module_data(module_data, name, arity)

    %{
      type: type,
      line: line,
      spec: {:attribute, 0, :type, spec},
      signature: signature
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

  def autolink_spec(ast, opts) do
    config = struct!(Autolink, opts)

    {name, quoted} =
      case ast do
        {:attribute, _, kind, {{name, _arity}, ast}} when kind in [:spec, :callback] ->
          {name, Enum.map(ast, &Code.Typespec.spec_to_quoted(name, &1))}

        {:attribute, _, kind, ast} when kind in [:type, :opaque] ->
          {name, _, _} = ast
          {name, Code.Typespec.type_to_quoted(ast)}
      end

    formatted = format_spec(ast)
    autolink_spec(quoted, name, formatted, config)
  end

  @impl true
  def highlight_info() do
    %{
      language_name: "erlang",
      lexer: Makeup.Lexers.ErlangLexer,
      opts: []
    }
  end

  ## Shared between Erlang & Elixir

  @doc false
  def get_abstract_code(module) do
    {^module, binary, _file} = :code.get_object_code(module)

    case :beam_lib.chunks(binary, [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_vsn, abstract_code}}]}} -> abstract_code
      _otherwise -> []
    end
  end

  @doc false
  def find_module_line(module, abst_code) do
    Enum.find_value(abst_code, fn
      {:attribute, anno, :module, ^module} -> anno_line(anno)
      _ -> nil
    end)
  end

  # Returns a map of {name, arity} => spec.
  def get_specs(module) do
    case Code.Typespec.fetch_specs(module) do
      {:ok, specs} -> Map.new(specs)
      :error -> %{}
    end
  end

  def get_callbacks(module) do
    case Code.Typespec.fetch_callbacks(module) do
      {:ok, callbacks} -> Map.new(callbacks)
      :error -> %{}
    end
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

          _ ->
            # TODO investigate building OTP docs
            ast
        end

      "https://erlang.org/doc/link/seemfa" ->
        case String.split(attrs[:href], ":") do
          [_] ->
            autolink(:function, attrs[:href], inner, config)

          [app, mfa] ->
            autolink_app({:function, app, mfa}, config)

          _ ->
            # TODO investigate building OTP docs
            ast
        end

      "https://erlang.org/doc/link/seetype" ->
        case String.split(attrs[:href], ":") do
          [_] ->
            autolink(:type, attrs[:href], inner, config)

          [app, mfa] ->
            autolink_app({:type, app, mfa}, config)
        end

      "https://erlang.org/doc/link/seeapp" ->
        case String.split(attrs[:href], ":") do
          [app, "index"] ->
            autolink_app({:app, app}, config)

          _ ->
            # TODO investigate building OTP docs
            ast
        end

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

  # Traverses quoted and formatted string of the typespec AST, replacing refs with links.
  #
  # Let's say we have this typespec:
  #
  #     -spec f(X) -> #{atom() => bar(), integer() => X}.
  #
  # We traverse the AST and find types and their string representations:
  #
  #     -spec f(X) -> #{atom() => bar(), integer() => X}.
  #                     ^^^^      ^^^    ^^^^^^^
  #
  #     atom/0    => atom
  #     bar/0     => bar
  #     integer/0 => integer
  #
  # We then traverse the formatted string, *in order*, replacing the type strings with links:
  #
  #     "atom("    => "atom("
  #     "bar("     => "<a>bar</a>("
  #     "integer(" => "integer("
  #
  # Finally we end up with:
  #
  #     -spec f(X) -> #{atom() => <a>bar</a>(), integer() => X}.
  #
  # All of this hassle is to preserve the original *text layout* of the initial representation,
  # all the spaces, newlines, etc.
  defp autolink_spec(quoted, name, formatted, config) do
    acc =
      for quoted <- List.wrap(quoted) do
        {_, acc} =
          Macro.prewalk(quoted, [], fn
            # module.name(args)
            {{:., _, [module, name]}, _, args}, acc ->
              {{:t, [], args}, [{pp({module, name}), {module, name, length(args)}} | acc]}

            {name, _, args} = ast, acc when is_atom(name) and is_list(args) ->
              arity = length(args)

              cond do
                name in [:"::", :when, :%{}, :{}, :|, :->, :record] ->
                  {ast, acc}

                # %{required(...) => ..., optional(...) => ...}
                name in [:required, :optional] and arity == 1 ->
                  {ast, acc}

                # name(args)
                true ->
                  {ast, [{pp(name), {name, arity}} | acc]}
              end

            other, acc ->
              {other, acc}
          end)

        acc |> Enum.reverse() |> Enum.drop(1)
      end
      |> Enum.concat()

    put(acc)

    # Drop and re-add type name (it, the first element in acc, is dropped there too)
    #
    #     1. foo() :: bar()
    #     2.     ) :: bar()
    #     3.     ) :: <a>bar</a>()
    #     4. foo() :: <a>bar</a>()
    name = pp(name)
    formatted = String.trim_leading(formatted, name <> "(")
    formatted = replace(formatted, acc, config)
    name <> "(" <> formatted
  end

  defp replace(formatted, [], _config) do
    formatted
  end

  defp replace(formatted, acc, config) do
    String.replace(formatted, Enum.map(acc, &"#{elem(&1, 0)}("), fn string ->
      string = String.trim_trailing(string, "(")
      {other, ref} = pop()

      if string != other do
        Autolink.maybe_warn(
          "internal inconsistency, please submit bug: #{inspect(string)} != #{inspect(other)}",
          config,
          nil,
          nil
        )
      end

      url =
        case ref do
          {name, arity} ->
            visibility = Refs.get_visibility({:type, config.current_module, name, arity})

            if visibility == :public do
              final_url({:type, name, arity}, config)
            end

          {module, name, arity} ->
            ref = {:type, module, name, arity}
            visibility = Refs.get_visibility(ref)

            if visibility == :public do
              final_url(ref, config)
            else
              original_text = "#{string}/#{arity}"
              Autolink.maybe_warn(ref, config, visibility, %{original_text: original_text})
              nil
            end
        end

      if url do
        ~s|<a href="#{url}">#{string}</a>(|
      else
        string <> "("
      end
    end)
  end

  defp put(items) do
    Process.put({__MODULE__, :stack}, items)
  end

  defp pop() do
    [head | tail] = Process.get({__MODULE__, :stack})
    put(tail)
    head
  end

  defp pp(name) when is_atom(name) do
    :io_lib.format("~p", [name]) |> IO.iodata_to_binary()
  end

  defp pp({module, name}) when is_atom(module) and is_atom(name) do
    :io_lib.format("~p:~p", [module, name]) |> IO.iodata_to_binary()
  end

  defp format_spec(ast) do
    {:attribute, _, type, _} = ast

    # `-type ` => 6
    offset = byte_size(Atom.to_string(type)) + 2

    options = [linewidth: 98 + offset]
    :erl_pp.attribute(ast, options) |> IO.iodata_to_binary() |> trim_offset(offset)
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

  defp anno_line(line) when is_integer(line), do: abs(line)
  defp anno_line(anno), do: anno |> :erl_anno.line() |> abs()
end
