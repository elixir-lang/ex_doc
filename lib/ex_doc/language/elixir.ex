defmodule ExDoc.Language.Elixir do
  @moduledoc false

  @behaviour ExDoc.Language

  alias ExDoc.Autolink
  alias ExDoc.Refs
  alias ExDoc.Language.Erlang

  @impl true
  def module_data(module, docs_chunk, config) do
    {type, skip} = module_type_and_skip(module)

    cond do
      skip ->
        :skip

      abst_code = Erlang.get_abstract_code(module) ->
        title = module_title(module, type)
        line = Erlang.find_module_line(module, abst_code)
        optional_callbacks = type == :behaviour && module.behaviour_info(:optional_callbacks)

        %{
          module: module,
          docs: docs_chunk,
          language: __MODULE__,
          id: inspect(module),
          title: title,
          type: type,
          line: line,
          callback_types: [:callback, :macrocallback],
          nesting_info: nesting_info(title, config.nest_modules_by_prefix),
          private: %{
            abst_code: abst_code,
            specs: Erlang.get_specs(module),
            callbacks: Erlang.get_callbacks(module),
            impls: get_impls(module),
            optional_callbacks: optional_callbacks
          }
        }

      true ->
        IO.warn("skipping docs for module #{inspect(module)}, reason: :no_debug_info", [])
    end
  end

  @impl true
  def function_data(entry, module_data) do
    {{kind, name, arity}, _anno, _signature, doc_content, metadata} = entry

    if doc?(entry, module_data.type) do
      function_data(kind, name, arity, doc_content, metadata, module_data)
    else
      :skip
    end
  end

  def function_data(kind, name, arity, _doc_content, metadata, module_data) do
    extra_annotations =
      case {kind, name, arity} do
        {:macro, _, _} -> ["macro"]
        {_, :__struct__, 0} -> ["struct"]
        _ -> []
      end

    actual_def = actual_def(name, arity, kind)

    %{
      doc_fallback: fn ->
        impl = Map.fetch(module_data.private.impls, actual_def)

        callback_doc_ast(name, arity, impl) ||
          delegate_doc_ast(metadata[:delegate_to])
      end,
      extra_annotations: extra_annotations,
      line: find_function_line(module_data, actual_def),
      specs: specs(kind, name, actual_def, module_data)
    }
  end

  # We are only interested in functions and macros for now
  defp doc?({{kind, _, _}, _, _, _, _}, _) when kind not in [:function, :macro] do
    false
  end

  # Skip impl_for and impl_for! for protocols
  defp doc?({{_, name, _}, _, _, _, _}, :protocol) when name in [:impl_for, :impl_for!] do
    false
  end

  # If content is a map, then it is ok.
  defp doc?({_, _, _, %{}, _}, _) do
    true
  end

  # If it is none, then we need to look at underscore.
  # TODO: We can remove this on Elixir v1.13 as all underscored are hidden.
  defp doc?({{_, name, _}, _, _, :none, _}, _type) do
    not match?([?_ | _], Atom.to_charlist(name))
  end

  # Everything else is hidden.
  defp doc?({_, _, _, _, _}, _) do
    false
  end

  @impl true
  def callback_data(entry, module_data) do
    {{kind, name, arity}, anno, _signature, _doc, _metadata} = entry
    actual_def = actual_def(name, arity, kind)

    extra_annotations =
      if actual_def in module_data.private.optional_callbacks, do: ["optional"], else: []

    specs =
      case module_data.private.callbacks do
        %{^actual_def => specs} when kind == :macrocallback ->
          Enum.map(specs, &remove_callback_term/1)

        %{^actual_def => specs} ->
          specs

        %{} ->
          []
      end

    line =
      if specs != [] do
        {:type, anno, _, _} = hd(specs)
        anno_line(anno)
      else
        anno_line(anno)
      end

    quoted = Enum.map(specs, &Code.Typespec.spec_to_quoted(name, &1))
    signature = [get_typespec_signature(hd(quoted), arity)]

    %{
      line: line,
      signature: signature,
      specs: quoted,
      extra_annotations: extra_annotations
    }
  end

  defp remove_callback_term({:type, num, :bounded_fun, [lhs, rhs]}) do
    {:type, num, :bounded_fun, [remove_callback_term(lhs), rhs]}
  end

  defp remove_callback_term({:type, num, :fun, [{:type, num, :product, [_ | rest_args]} | rest]}) do
    {:type, num, :fun, [{:type, num, :product, rest_args} | rest]}
  end

  @impl true
  def type_data(entry, module_data) do
    {{_kind, name, arity}, _anno, _signature, _doc, _metadata} = entry

    %{type: type, spec: spec, line: line} = type_from_module_data(module_data, name, arity)
    quoted = spec |> Code.Typespec.type_to_quoted() |> process_type_ast(type)
    signature = [get_typespec_signature(quoted, arity)]

    %{
      type: type,
      line: line,
      spec: quoted,
      signature: signature
    }
  end

  @doc false
  def type_from_module_data(module_data, name, arity) do
    Enum.find_value(module_data.private.abst_code, fn
      {:attribute, anno, type, {^name, _, args} = spec} ->
        if type in [:opaque, :type] and length(args) == arity do
          %{
            type: type,
            spec: spec,
            line: anno_line(anno)
          }
        end

      _ ->
        nil
    end)
  end

  @impl true
  def autolink_doc(ast, opts) do
    config = struct!(Autolink, opts)
    walk_doc(ast, config)
  end

  @impl true
  def autolink_spec(ast, opts) do
    config = struct!(Autolink, opts)

    # TODO: re-use ExDoc.Language.Erlang.autolink_spec/2

    string =
      ast
      |> Macro.to_string()
      |> safe_format_string!()
      |> ExDoc.Utils.h()

    name = typespec_name(ast)
    {name, rest} = split_name(string, name)

    name <> do_typespec(rest, config)
  end

  @impl true
  def highlight_info() do
    %{
      language_name: "elixir",
      lexer: Makeup.Lexers.ElixirLexer,
      opts: []
    }
  end

  @impl true
  def format_spec_attribute(%ExDoc.TypeNode{type: type}), do: "@#{type}"
  def format_spec_attribute(%ExDoc.FunctionNode{type: :callback}), do: "@callback"
  def format_spec_attribute(%ExDoc.FunctionNode{type: :macrocallback}), do: "@macrocallback"
  def format_spec_attribute(%ExDoc.FunctionNode{}), do: "@spec"

  ## Module Helpers

  defp nesting_info(title, prefixes) do
    prefixes
    |> Enum.find(&String.starts_with?(title, &1 <> "."))
    |> case do
      nil -> nil
      prefix -> {"." <> String.trim_leading(title, prefix <> "."), prefix}
    end
  end

  defp module_type_and_skip(module) do
    cond do
      function_exported?(module, :__struct__, 0) and
          match?(%{__exception__: true}, module.__struct__) ->
        {:exception, false}

      function_exported?(module, :__protocol__, 1) ->
        {:protocol, false}

      function_exported?(module, :__impl__, 1) ->
        {:impl, true}

      match?("Elixir.Mix.Tasks." <> _, Atom.to_string(module)) ->
        {:task, false}

      function_exported?(module, :behaviour_info, 1) ->
        {:behaviour, false}

      true ->
        {:module, false}
    end
  end

  defp module_title(module, :task), do: "mix " <> task_name(module)
  defp module_title(module, _), do: inspect(module)

  defp task_name(module) do
    "Elixir.Mix.Tasks." <> name = Atom.to_string(module)
    name |> String.split(".") |> Enum.map_join(".", &Macro.underscore/1)
  end

  def get_impls(module) do
    for behaviour <- behaviours_implemented_by(module),
        {callback, _} <- Erlang.get_callbacks(behaviour),
        do: {callback, behaviour},
        into: %{}
  end

  defp behaviours_implemented_by(module) do
    for {:behaviour, list} <- module.module_info(:attributes),
        behaviour <- list,
        do: behaviour
  end

  ## Helpers

  defp specs(kind, name, actual_def, module_data) do
    specs =
      module_data.private.specs
      |> Map.get(actual_def, [])
      |> Enum.map(&Code.Typespec.spec_to_quoted(name, &1))

    if kind == :macro do
      Enum.map(specs, &remove_first_macro_arg/1)
    else
      specs
    end
  end

  defp actual_def(name, arity, :macrocallback) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  defp actual_def(name, arity, :macro) do
    {String.to_atom("MACRO-" <> to_string(name)), arity + 1}
  end

  defp actual_def(name, arity, _), do: {name, arity}

  defp remove_first_macro_arg({:"::", info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    {:"::", info, [{name, info2, rest_args}, return]}
  end

  defp remove_first_macro_arg({:when, meta, [lhs, rhs]}) do
    {:when, meta, [remove_first_macro_arg(lhs), rhs]}
  end

  defp delegate_doc_ast({m, f, a}) do
    [
      {:p, [], ["See ", {:code, [class: "inline"], [Exception.format_mfa(m, f, a)], %{}}, "."],
       %{}}
    ]
  end

  defp delegate_doc_ast(nil) do
    nil
  end

  defp callback_doc_ast(name, arity, {:ok, behaviour}) do
    [
      {:p, [],
       [
         "Callback implementation for ",
         {:code, [class: "inline"], ["c:#{inspect(behaviour)}.#{name}/#{arity}"], %{}},
         "."
       ], %{}}
    ]
  end

  defp callback_doc_ast(_, _, _) do
    nil
  end

  defp find_function_line(module_data, {name, arity}) do
    Enum.find_value(module_data.private.abst_code, fn
      {:function, anno, ^name, ^arity, _} -> anno_line(anno)
      _ -> nil
    end)
  end

  defp anno_line(line) when is_integer(line), do: abs(line)
  defp anno_line(anno), do: anno |> :erl_anno.line() |> abs()

  defp get_typespec_signature({:when, _, [{:"::", _, [{name, meta, args}, _]}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({:"::", _, [{name, meta, args}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({name, meta, args}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp strip_types(args, arity) do
    args
    |> Enum.take(-arity)
    |> Enum.with_index(1)
    |> Enum.map(fn
      {{:"::", _, [left, _]}, position} -> to_var(left, position)
      {{:|, _, _}, position} -> to_var({}, position)
      {left, position} -> to_var(left, position)
    end)
    |> Macro.prewalk(fn node -> Macro.update_meta(node, &Keyword.delete(&1, :line)) end)
  end

  defp to_var({:%, meta, [name, _]}, _), do: {:%, meta, [name, {:%{}, meta, []}]}
  defp to_var({:%{}, _, _}, _), do: {:map, [], nil}
  defp to_var({name, meta, _}, _) when is_atom(name), do: {name, meta, nil}

  defp to_var({{:., meta, [_module, name]}, _, _args}, _) when is_atom(name),
    do: {name, meta, nil}

  defp to_var([{:->, _, _} | _], _), do: {:function, [], nil}
  defp to_var({:<<>>, _, _}, _), do: {:binary, [], nil}
  defp to_var({:{}, _, _}, _), do: {:tuple, [], nil}
  defp to_var({_, _}, _), do: {:tuple, [], nil}
  defp to_var(integer, _) when is_integer(integer), do: {:integer, [], nil}
  defp to_var(float, _) when is_integer(float), do: {:float, [], nil}
  defp to_var(list, _) when is_list(list), do: {:list, [], nil}
  defp to_var(atom, _) when is_atom(atom), do: {:atom, [], nil}
  defp to_var(_, position), do: {:"arg#{position}", [], nil}

  # Cut off the body of an opaque type while leaving it on a normal type.
  defp process_type_ast({:"::", _, [d | _]}, :opaque), do: d
  defp process_type_ast(ast, _), do: ast

  ## Autolinking

  @autoimported_modules [Kernel, Kernel.SpecialForms]

  defp walk_doc(list, config) when is_list(list) do
    Enum.map(list, &walk_doc(&1, config))
  end

  defp walk_doc(binary, _) when is_binary(binary) do
    binary
  end

  defp walk_doc({:pre, _, _, _} = ast, _config) do
    ast
  end

  defp walk_doc({:a, attrs, inner, meta} = ast, config) do
    case custom_link(attrs, config) do
      :remove_link ->
        remove_link(ast)

      nil ->
        ast

      url ->
        {:a, Keyword.put(attrs, :href, url), inner, meta}
    end
  end

  defp walk_doc({:code, attrs, [code], meta} = ast, config) do
    if url = url(code, :regular_link, config) do
      code = remove_prefix(code)
      {:a, [href: url], [{:code, attrs, [code], meta}], %{}}
    else
      ast
    end
  end

  defp walk_doc({tag, attrs, ast, meta}, config) do
    {tag, attrs, List.flatten(walk_doc(ast, config)), meta}
  end

  defp remove_link({:a, _attrs, inner, _meta}) do
    inner
  end

  @ref_regex ~r/^`(.+)`$/

  defp custom_link(attrs, config) do
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

  defp remove_and_warn_if_invalid(nil, reference, config) do
    warn(
      ~s[documentation references "#{reference}" but it is invalid],
      {config.file, config.line},
      config.id
    )

    :remove_link
  end

  defp remove_and_warn_if_invalid(result, _, _), do: result

  defp warn(message, {file, line}, id) do
    warning = IO.ANSI.format([:yellow, "warning: ", :reset])

    stacktrace =
      "  #{file}" <>
        if(line, do: ":#{line}", else: "") <>
        if(id, do: ": #{id}", else: "")

    IO.puts(:stderr, [warning, message, ?\n, stacktrace, ?\n])
  end

  defp build_extra_link(link, config) do
    with %{scheme: nil, host: nil, path: path} = uri <- URI.parse(link),
         true <- is_binary(path) and path != "" and not (path =~ @ref_regex),
         true <- Path.extname(path) in [".livemd", ".md", ".txt", ""] do
      if file = config.extras[Path.basename(path)] do
        fragment = (uri.fragment && "#" <> uri.fragment) || ""
        file <> config.ext <> fragment
      else
        Autolink.maybe_warn(nil, config, nil, %{file_path: path, original_text: link})
        nil
      end
    else
      _ -> nil
    end
  end

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

  defp url(string = "mix help " <> name, mode, config) do
    name |> mix_task(string, mode, config) |> maybe_remove_link(mode)
  end

  defp url(string = "mix " <> name, mode, config) do
    name |> mix_task(string, mode, config) |> maybe_remove_link(mode)
  end

  defp url(string, mode, config) do
    if Enum.any?(config.skip_code_autolink_to, &(&1 == string)) do
      nil
    else
      parse_url(string, mode, config)
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

  # Remove link when we fail to parse reference so we don't warn twice
  defp maybe_remove_link(nil, :custom_link) do
    :remove_link
  end

  defp maybe_remove_link(result, _mode) do
    result
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
      Autolink.maybe_warn({:module, module}, config, visibility, %{
        mix_task: true,
        original_text: string
      })
    end

    url
  end

  defp safe_format_string!(string) do
    try do
      string
      |> Code.format_string!(line_length: 80)
      |> IO.iodata_to_binary()
    rescue
      _ -> string
    end
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

    Regex.replace(regex, string, fn all, call_string, module_string, name_string, rest ->
      module = string_to_module(module_string)
      name = String.to_atom(name_string)
      arity = count_args(rest, 0, 0)
      original_text = call_string <> "()"

      if Enum.any?(config.filtered_modules, &(&1.id == module_string)) do
        warn("Typespec references filtered module: #{all}", {config.file, config.line}, config.id)
      end

      url =
        if module do
          remote_url({:type, module, name, arity}, config, original_text)
        else
          local_url(:type, name, arity, config, original_text)
        end

      if url do
        ~s[<a href="#{url}">#{ExDoc.Utils.h(call_string)}</a>]
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

  defp module_url(module, mode, config, string) do
    ref = {:module, module}

    case {mode, Refs.get_visibility(ref)} do
      {_link_type, visibility} when visibility in [:public, :limited] ->
        Autolink.app_module_url(Autolink.tool(module, config), module, config)

      {:regular_link, :undefined} ->
        nil

      {:custom_link, visibility} when visibility in [:hidden, :undefined] ->
        Autolink.maybe_warn(ref, config, visibility, %{original_text: string})
        :remove_link

      {_link_type, visibility} ->
        Autolink.maybe_warn(ref, config, visibility, %{original_text: string})
        nil
    end
  end

  defp local_url(kind, name, arity, config, original_text, options \\ [])

  defp local_url(:type, name, arity, config, _original_text, _options)
       when {name, arity} in @basic_types do
    Autolink.ex_doc_app_url(Kernel, config, "typespecs", config.ext, "#basic-types")
  end

  defp local_url(:type, name, arity, config, _original_text, _options)
       when {name, arity} in @built_in_types do
    Autolink.ex_doc_app_url(Kernel, config, "typespecs", config.ext, "#built-in-types")
  end

  defp local_url(kind, name, arity, config, original_text, options) do
    module = config.current_module
    ref = {kind, module, name, arity}
    mode = Keyword.get(options, :mode, :regular_link)
    visibility = Refs.get_visibility(ref)

    case {kind, visibility} do
      {_kind, :public} ->
        fragment(Autolink.tool(module, config), kind, name, arity)

      {:function, _visibility} ->
        try_autoimported_function(name, arity, mode, config, original_text)

      {:type, :hidden} ->
        nil

      {:type, _} ->
        nil

      _ ->
        Autolink.maybe_warn(ref, config, visibility, %{original_text: original_text})
        nil
    end
  end

  defp try_autoimported_function(name, arity, mode, config, original_text) do
    Enum.find_value(@autoimported_modules, fn module ->
      remote_url({:function, module, name, arity}, config, original_text,
        warn?: false,
        mode: mode
      )
    end)
  end

  defp remote_url({kind, module, name, arity} = ref, config, original_text, opts \\ []) do
    warn? = Keyword.get(opts, :warn?, true)
    mode = Keyword.get(opts, :mode, :regular_link)
    same_module? = module == config.current_module

    case {mode, Refs.get_visibility({:module, module}), Refs.get_visibility(ref)} do
      {_mode, _module_visibility, :public} ->
        tool = Autolink.tool(module, config)

        if same_module? do
          fragment(tool, kind, name, arity)
        else
          Autolink.app_module_url(tool, module, config) <> fragment(tool, kind, name, arity)
        end

      {:regular_link, module_visibility, :undefined}
      when module_visibility == :public
      when module_visibility == :limited and kind != :type ->
        if warn?,
          do: Autolink.maybe_warn(ref, config, :undefined, %{original_text: original_text})

        nil

      {:regular_link, _module_visibility, :undefined} when not same_module? ->
        nil

      {_mode, _module_visibility, visibility} ->
        if warn?,
          do: Autolink.maybe_warn(ref, config, visibility, %{original_text: original_text})

        nil
    end
  end

  defp prefix(kind)
  defp prefix(:function), do: ""
  defp prefix(:callback), do: "c:"
  defp prefix(:type), do: "t:"

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
end
