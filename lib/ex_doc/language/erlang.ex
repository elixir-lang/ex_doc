defmodule ExDoc.Language.Erlang do
  @moduledoc false

  @behaviour ExDoc.Language

  alias ExDoc.{Autolink, Refs}

  @impl true
  @spec module_data(atom, any, any) ::
          :ok
          | %{
              callback_types: [:callback, ...],
              docs: any,
              id: binary,
              language: ExDoc.Language.Erlang,
              line: any,
              module: atom,
              nesting_info: nil,
              private: %{abst_code: any, callbacks: map, optional_callbacks: any, specs: map},
              title: binary,
              type: :behaviour | :module
            }
  def module_data(module, docs_chunk, _config) do
    if abst_code = get_abstract_code(module) do
      id = Atom.to_string(module)
      line = find_module_line(module, abst_code)
      type = module_type(module)

      optional_callbacks =
        type == :behaviour &&
          try do
            module.behaviour_info(:optional_callbacks)
          rescue
            FunctionClauseError -> :undefined
          end

      optional_callbacks =
        case optional_callbacks do
          :undefined -> []
          _ -> optional_callbacks
        end

      %{
        module: module,
        docs: docs_chunk,
        language: __MODULE__,
        id: id,
        title: id,
        type: type,
        line: line,
        callback_types: [:callback],
        nesting_info: nil,
        private: %{
          abst_code: abst_code,
          specs: get_specs(module),
          callbacks: get_callbacks(module),
          optional_callbacks: optional_callbacks
        }
      }
    else
      IO.warn("skipping docs for module #{inspect(module)}, reason: :no_debug_info", [])
    end
  end

  @impl true
  def function_data(entry, module_data) do
    {{kind, name, arity}, _anno, _signature, doc_content, _metadata} = entry

    # TODO: Edoc on Erlang/OTP24.1+ includes private functions in
    # the chunk, so we manually yank them out for now.
    if kind == :function and doc_content != :hidden and
         function_exported?(module_data.module, name, arity) do
      function_data(name, arity, doc_content, module_data)
    else
      :skip
    end
  end

  defp function_data(name, arity, _doc_content, module_data) do
    specs =
      case Map.fetch(module_data.private.specs, {name, arity}) do
        {:ok, specs} ->
          [{:attribute, 0, :spec, {{name, arity}, specs}}]

        :error ->
          case Map.fetch(module_data.private.specs, {module_data.module, name, arity}) do
            {:ok, specs} ->
              [{:attribute, 0, :spec, {{module_data.module, name, arity}, specs}}]

            :error ->
              []
          end
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

    extra_annotations =
      if {name, arity} in module_data.private.optional_callbacks, do: ["optional"], else: []

    specs =
      case Map.fetch(module_data.private.callbacks, {name, arity}) do
        {:ok, specs} ->
          [{:attribute, 0, :callback, {{name, arity}, specs}}]

        :error ->
          []
      end

    %{
      line: anno_line(anno),
      signature: signature,
      specs: specs,
      extra_annotations: extra_annotations
    }
  end

  @impl true
  def type_data(entry, module_data) do
    {{kind, name, arity}, anno, signature, _doc, _metadata} = entry

    case ExDoc.Language.Elixir.type_from_module_data(module_data, name, arity) do
      %{} = map ->
        %{
          type: map.type,
          line: map.line,
          spec: {:attribute, 0, map.type, map.spec},
          signature: signature
        }

      nil ->
        %{
          type: kind,
          line: anno_line(anno),
          spec: nil,
          signature: signature
        }
    end
  end

  @impl true
  def autolink_doc(ast, opts) do
    config = struct!(Autolink, opts)
    true = config.language == __MODULE__

    config = %{config | force_module_prefix: true}
    walk_doc(ast, config)
  end

  @impl true
  def autolink_spec(nil, _opts) do
    nil
  end

  def autolink_spec({:attribute, _, :opaque, ast}, _opts) do
    {name, _, args} = ast

    args =
      for arg <- args do
        {:var, _, name} = arg
        Atom.to_string(name)
      end
      |> Enum.intersperse(", ")

    IO.iodata_to_binary([Atom.to_string(name), "(", args, ")"])
  end

  def autolink_spec(ast, opts) do
    config = struct!(Autolink, opts)

    {name, quoted} =
      case ast do
        {:attribute, _, kind, {mfa, ast}} when kind in [:spec, :callback] ->
          {mn, name} =
            case mfa do
              {name, _} -> {name, name}
              {module, name, _} -> {{module, name}, name}
            end

          {mn, Enum.map(ast, &Code.Typespec.spec_to_quoted(name, &1))}

        {:attribute, _, :type, ast} ->
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

  @impl true
  def format_spec_attribute(%ExDoc.TypeNode{type: type}), do: "-#{type}"
  def format_spec_attribute(%ExDoc.FunctionNode{type: :callback}), do: "-callback"
  def format_spec_attribute(%ExDoc.FunctionNode{}), do: "-spec"

  ## Shared between Erlang & Elixir

  @doc false
  def get_abstract_code(module) do
    with {^module, binary, _file} <- :code.get_object_code(module),
         {:ok, {_, [{:abstract_code, {_vsn, abstract_code}}]}} <-
           :beam_lib.chunks(binary, [:abstract_code]) do
      abstract_code
    else
      _ -> nil
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

  defp walk_doc({:pre, _, _, _} = ast, _config) do
    ast
  end

  defp walk_doc({:code, attrs, [code], meta} = ast, config) when is_binary(code) do
    case Autolink.url(code, :regular_link, config) do
      url when is_binary(url) ->
        code = remove_prefix(code)
        {:a, [href: url], [{:code, attrs, [code], meta}], %{}}

      _ ->
        ast
    end
  end

  defp walk_doc({:a, attrs, inner, meta} = ast, config) do
    case attrs[:rel] do
      "https://erlang.org/doc/link/seeerl" ->
        {fragment, url} = extract_fragment(attrs[:href] || "", "#")

        case String.split(url, ":") do
          [module] ->
            walk_doc({:a, [href: "`m:#{module}#{fragment}`"], inner, meta}, config)

          [app, module] ->
            inner = strip_app(inner, app)
            walk_doc({:a, [href: "`m:#{module}#{fragment}`"], inner, meta}, config)

          _ ->
            warn_ref(attrs[:href], config)
            inner
        end

      "https://erlang.org/doc/link/seemfa" ->
        {prefix, url} =
          case String.split(attrs[:href], "Module:") do
            [url] ->
              {"", url}

            [left, right] ->
              {"c:", left <> right}
          end

        {mfa, inner} =
          case String.split(url, ":") do
            [mfa] ->
              {mfa, inner}

            [app, mfa] ->
              {mfa, strip_app(inner, app)}
          end

        walk_doc({:a, [href: "`#{prefix}#{fixup(mfa)}`"], inner, meta}, config)

      "https://erlang.org/doc/link/seetype" ->
        {type, inner} =
          case String.split(attrs[:href], ":") do
            [type] ->
              {type, inner}

            [app, type] ->
              {type, strip_app(inner, app)}
          end

        type =
          case String.split(type, "(") do
            [type] ->
              type

            [type, _] ->
              type <> "/0"
          end

        walk_doc({:a, [href: "`t:#{fixup(type)}`"], inner, meta}, config)

      "https://erlang.org/doc/link/" <> see ->
        warn_ref(attrs[:href] <> " (#{see})", config)
        inner

      _ ->
        case Autolink.custom_link(attrs, config) do
          :remove_link ->
            remove_link(ast)

          nil ->
            ast

          url ->
            {:a, Keyword.put(attrs, :href, url), inner, meta}
        end
    end
  end

  defp walk_doc({tag, attrs, ast, meta}, config) do
    {tag, attrs, walk_doc(ast, config), meta}
  end

  defp remove_link({:a, _attrs, inner, _meta}) do
    inner
  end

  defp remove_prefix("c:" <> rest), do: rest
  defp remove_prefix("m:" <> rest), do: rest
  defp remove_prefix("t:" <> rest), do: rest
  defp remove_prefix("\\" <> rest), do: rest
  defp remove_prefix(rest), do: rest

  defp extract_fragment(url, prefix) do
    case String.split(url, "#", parts: 2) do
      [url] -> {"", url}
      [url, fragment] -> {prefix <> fragment, url}
    end
  end

  defp fixup(mfa) do
    case String.split(mfa, "#") do
      ["", mfa] ->
        mfa

      [m, fa] ->
        m <> ":" <> fa
    end
  end

  defp strip_app([{:code, attrs, [code], meta}], app) do
    [{:code, attrs, strip_app(code, app), meta}]
  end

  defp strip_app(code, app) when is_binary(code) do
    String.trim_leading(code, "//#{app}/")
  end

  defp strip_app(other, _app) do
    other
  end

  defp warn_ref(href, config) do
    message = "invalid reference: #{href}"
    nil = config.id
    Autolink.maybe_warn(config, message, nil, %{})
  end

  defp final_url({kind, name, arity}, _config) do
    Autolink.fragment(:ex_doc, kind, name, arity)
  end

  defp final_url({kind, module, name, arity}, config) do
    tool = Autolink.tool(module, config)
    Autolink.app_module_url(tool, module, Autolink.fragment(tool, kind, name, arity), config)
  end

  @impl true
  def parse_module_function(string) do
    case String.split(string, ":") do
      [module_string, function_string] ->
        with {:module, module} <- parse_module_string(module_string, :custom_link),
             {:function, function} <- parse_function(function_string) do
          {:remote, module, function}
        end

      [function_string] ->
        with {:function, function} <- parse_function(function_string) do
          {:local, function}
        end

      _ ->
        :error
    end
  end

  defp parse_function(string) do
    with {:ok, toks, _} <- :erl_scan.string(String.to_charlist("fun #{string}/0.")),
         {:ok, [{:fun, _, {:function, name, _arity}}]} <- :erl_parse.parse_exprs(toks) do
      {:function, name}
    else
      _ ->
        :error
    end
  end

  @impl true
  def try_autoimported_function(name, arity, mode, config, original_text) do
    if :erl_internal.bif(name, arity) do
      Autolink.remote_url({:function, :erlang, name, arity}, config, original_text,
        warn?: false,
        mode: mode
      )
    end
  end

  @impl true
  def try_builtin_type(name, arity, mode, config, original_text) do
    if :erl_internal.is_type(name, arity) do
      Autolink.remote_url({:type, :erlang, name, arity}, config, original_text,
        warn?: false,
        mode: mode
      )
    end
  end

  @impl true
  def parse_module(string, mode) do
    case String.split(string, "#", parts: 2) do
      [mod, anchor] ->
        case parse_module_string(mod, mode) do
          {:module, mod} ->
            {:module, mod, anchor}

          :error ->
            :error
        end

      [mod] ->
        parse_module_string(mod, mode)
    end
  end

  def parse_module_string(string, _mode) do
    case Code.string_to_quoted(":'#{string}'",
           warn_on_unnecessary_quotes: false,
           emit_warnings: false
         ) do
      {:ok, module} when is_atom(module) ->
        {:module, module}

      _ ->
        :error
    end
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
        {_quoted, acc} =
          Macro.prewalk(quoted, [], fn
            # module.name(args)
            {{:., _, [module, name]}, _, args}, acc ->
              {{:t, [], args}, [{pp({module, name}), {module, name, length(args)}} | acc]}

            {name, _, _}, acc when name in [:<<>>, :..] ->
              {nil, acc}

            # -1
            {:-, _, [int]}, acc when is_integer(int) ->
              {nil, acc}

            # fun() (spec_to_quoted expands it to (... -> any())
            {:->, _, [[{name, _, _}], {:any, _, _}]}, acc when name == :... ->
              {nil, acc}

            # record{type :: remote:type/arity}
            {:field_type, _, [name, {{:., _, [r_mod, r_type]}, _, args}]}, acc ->
              {name, [{pp({r_mod, r_type}), {r_mod, r_type, length(args)}} | acc]}

            # #{x :: t()}
            {:field_type, _, [name, type]}, acc when is_atom(name) ->
              {[type], acc}

            {name, _, args} = ast, acc when is_atom(name) and is_list(args) ->
              arity = length(args)

              cond do
                name == :record and acc != [] ->
                  {ast, acc}

                name in [:"::", :when, :%{}, :{}, :|, :->] ->
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

        acc
        |> Enum.reverse()
        # drop the name of the typespec
        |> Enum.drop(1)
      end
      |> Enum.concat()

    put(acc)

    # Drop and re-add type name (it, the first element in acc, is dropped there too)
    #
    #     1. foo() :: bar()
    #     2.    () :: bar()
    #     3.    () :: <a>bar</a>()
    #     4. foo() :: <a>bar</a>()
    name = pp(name)
    formatted = trim_name(formatted, name)
    formatted = replace(formatted, acc, config)
    name <> formatted
  end

  defp trim_name(string, name) do
    name_size = byte_size(name)
    binary_part(string, name_size, byte_size(string) - name_size)
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
          config,
          "internal inconsistency, please submit bug: #{inspect(string)} != #{inspect(other)}",
          nil,
          nil
        )
      end

      url =
        case ref do
          {name, arity} ->
            visibility = Refs.get_visibility({:type, config.current_module, name, arity})

            cond do
              visibility in [:public, :hidden] ->
                final_url({:type, name, arity}, config)

              :erl_internal.is_type(name, arity) ->
                final_url({:type, :erlang, name, arity}, config)

              true ->
                nil
            end

          {module, name, arity} ->
            ref = {:type, module, name, arity}
            visibility = Refs.get_visibility(ref)

            if visibility in [:public, :hidden] do
              final_url(ref, config)
            else
              original_text = "#{string}/#{arity}"
              Autolink.maybe_warn(config, ref, visibility, %{original_text: original_text})
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
