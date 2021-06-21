defmodule ExDoc.Language.Elixir do
  @moduledoc false

  @behaviour ExDoc.Language

  alias ExDoc.Autolink
  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates, as: T

  @impl true
  def module_data(module) do
    {type, skip} = module_type_and_skip(module)

    %{
      id: inspect(module),
      title: module_title(module, type),
      type: type,
      skip: skip,
      extra_callback_types: [:macrocallback]
    }
  end

  @impl true
  def function_data(entry, module_data) do
    {{kind, name, arity}, _anno, _signature, _doc_content, metadata} = entry

    extra_annotations =
      case {kind, name, arity} do
        {:macro, _, _} -> ["macro"]
        {_, :__struct__, 0} -> ["struct"]
        _ -> []
      end

    actual_def = actual_def(name, arity, kind)

    %{
      doc_fallback: fn ->
        impl = Map.fetch(module_data.impls, actual_def)

        callback_doc_ast(name, arity, impl) ||
          delegate_doc_ast(metadata[:delegate_to])
      end,
      extra_annotations: extra_annotations,
      line: find_function_line(module_data, actual_def),
      specs: specs(kind, name, actual_def, module_data)
    }
  end

  @impl true
  def callback_data(entry, module_data) do
    {{kind, name, arity}, _anno, _signature, _doc, _metadata} = entry
    actual_def = actual_def(name, arity, kind)

    specs =
      case Map.fetch(module_data.callbacks, actual_def) do
        {:ok, specs} ->
          specs

        :error ->
          []
      end

    line =
      if specs != [] do
        {:type, anno, _, _} = hd(specs)
        anno_line(anno)
      end

    specs = Enum.map(specs, &Code.Typespec.spec_to_quoted(name, &1))

    %{
      actual_def: actual_def,
      line: line,
      signature_fallback: fn ->
        if specs != [] do
          get_typespec_signature(hd(specs), arity)
        end
      end,
      specs: specs
    }
  end

  @impl true
  def type_data(entry, spec) do
    {{kind, _name, arity}, _anno, _signature, _doc, _metadata} = entry
    spec = spec |> Code.Typespec.type_to_quoted() |> process_type_ast(kind)

    %{
      spec: spec,
      signature_fallback: fn ->
        get_typespec_signature(spec, arity)
      end
    }
  end

  @impl true
  def autolink_doc(doc, config) do
    walk(doc, config)
  end

  @impl true
  def autolink_spec(ast, config) do
    string =
      ast
      |> Macro.to_string()
      |> safe_format_string!()
      |> T.h()

    name = typespec_name(ast)
    {name, rest} = split_name(string, name)

    name <> do_typespec(rest, config)
  end

  ## Helpers

  defp module_type_and_skip(module) do
    cond do
      function_exported?(module, :__struct__, 0) and
          match?(%{__exception__: true}, module.__struct__) ->
        {:exception, false}

      function_exported?(module, :__protocol__, 1) ->
        {:protocol, false}

      function_exported?(module, :__impl__, 1) ->
        {:impl, true}

      function_exported?(module, :behaviour_info, 1) ->
        {:behaviour, false}

      match?("Elixir.Mix.Tasks." <> _, Atom.to_string(module)) ->
        {:task, false}

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

  defp specs(kind, name, actual_def, module_data) do
    specs =
      module_data.specs
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

  defp find_function_line(%{abst_code: abst_code}, {name, arity}) do
    Enum.find_value(abst_code, fn
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
  end

  defp to_var({:%, meta, [name, _]}, _), do: {:%, meta, [name, {:%{}, meta, []}]}
  defp to_var({name, meta, _}, _) when is_atom(name), do: {name, meta, nil}

  defp to_var({{:., meta, [_module, name]}, _, _args}, _) when is_atom(name),
    do: {name, meta, nil}

  defp to_var([{:->, _, _} | _], _), do: {:function, [], nil}
  defp to_var({:<<>>, _, _}, _), do: {:binary, [], nil}
  defp to_var({:%{}, _, _}, _), do: {:map, [], nil}
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

  defp walk(list, config) when is_list(list) do
    Enum.map(list, &walk(&1, config))
  end

  defp walk(binary, _) when is_binary(binary) do
    binary
  end

  defp walk({:pre, _, _, _} = ast, _config) do
    ast
  end

  defp walk({:a, attrs, inner, meta} = ast, config) do
    href = attrs[:href] || ""

    case Regex.run(~r/^`(.+)`$/, href) do
      [_, code] ->
        case url(code, :all, config) do
          {:ok, url} ->
            attrs = Keyword.put(attrs, :href, url)
            [{:a, attrs, inner, meta}]

          :error ->
            inner
        end

      _ ->
        case extras(href, config) do
          {:ok, url} ->
            attrs = Keyword.put(attrs, :href, url)
            [{:a, attrs, inner, meta}]

          :error ->
            ast
        end
    end
  end

  defp walk({:code, _attrs, [code], _meta} = ast, config) do
    case url(code, :public_module, config) do
      {:ok, url} ->
        [{:a, [href: url], [remove_prefix(ast)], %{}}]

      :error ->
        remove_prefix(ast)
    end
  end

  defp walk({tag, attrs, ast, meta}, config) do
    {tag, attrs, walk(ast, config), meta}
  end

  defp url(code, mode, config) do
    with {:ok, ref} <- ref(code) do
      case Autolink.url(ref, code, mode, config) do
        {:ok, url} ->
          {:ok, url}

        {:warn, warning} ->
          case try_autoimported(ref, code, config) do
            {:ok, url} ->
              {:ok, url}

            _ ->
              Autolink.maybe_warn(warning, config)
              :error
          end

        :error ->
          try_autoimported(ref, code, config)
      end
    end
  end

  defp ref("mix help " <> name), do: mix_task(name)
  defp ref("mix " <> name), do: mix_task(name)

  defp ref(string) do
    case Regex.run(~r{^(.+)/(\d+)$}, string) do
      [_, left, right] ->
        parse_module_function(left, String.to_integer(right))

      nil ->
        with {:ok, module} <- parse_module(string) do
          {:ok, {:module, module}}
        end

      _ ->
        :error
    end
  end

  defp parse_module_function(string, arity) do
    {kind, rest} = parse_kind(string)

    case rest |> String.split(".") |> Enum.reverse() do
      [string] ->
        with {:ok, function} <- parse_function(string) do
          {:ok, {kind, function, arity}}
        end

      ["", "", ""] ->
        {:ok, {kind, :.., arity}}

      ["//", "", ""] ->
        {:ok, {kind, :"..//", arity}}

      ["", ""] ->
        {:ok, {kind, :., arity}}

      ["", "", "" | rest] ->
        module_string = rest |> Enum.reverse() |> Enum.join(".")

        with {:ok, module} <- parse_module(module_string) do
          {:ok, {kind, module, :.., arity}}
        end

      ["", "" | rest] ->
        module_string = rest |> Enum.reverse() |> Enum.join(".")

        with {:ok, module} <- parse_module(module_string) do
          {:ok, {kind, module, :., arity}}
        end

      [function_string | rest] ->
        module_string = rest |> Enum.reverse() |> Enum.join(".")

        with {:ok, module} <- parse_module(module_string),
             {:ok, function} <- parse_function(function_string) do
          {:ok, {kind, module, function, arity}}
        end
    end
  end

  defp parse_kind("c:" <> rest), do: {:callback, rest}
  defp parse_kind("t:" <> rest), do: {:type, rest}
  defp parse_kind(rest), do: {:function, rest}

  defp parse_module(<<first>> <> _ = string) when first in ?A..?Z do
    do_parse_module(string)
  end

  defp parse_module(":" <> _ = string) do
    do_parse_module(string)
  end

  defp parse_module(_) do
    :error
  end

  defp do_parse_module(string) do
    case Code.string_to_quoted(string, warn_on_unnecessary_quotes: false) do
      {:ok, module} when is_atom(module) ->
        {:ok, module}

      {:ok, {:__aliases__, _, parts}} ->
        if Enum.all?(parts, &is_atom/1) do
          {:ok, Module.concat(parts)}
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp parse_function(string) do
    case Code.string_to_quoted(":" <> string) do
      {:ok, function} when is_atom(function) -> {:ok, function}
      _ -> :error
    end
  end

  defp mix_task(name) do
    if name =~ ~r/^[a-z][a-z0-9]+(\.[a-z][a-z0-9]+)*$/ do
      parts = name |> String.split(".") |> Enum.map(&Macro.camelize/1)
      {:ok, {:module, Module.concat([Mix, Tasks | parts])}}
    else
      :error
    end
  end

  defp try_autoimported({:function, name, arity}, string, config) do
    Enum.reduce_while([Kernel, Kernel.SpecialForms], :error, fn module, _ ->
      case Autolink.url({:function, module, name, arity}, string, :none, config) do
        {:ok, url} ->
          {:halt, {:ok, url}}

        :error ->
          {:cont, :error}
      end
    end)
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

  defp try_autoimported({:type, name, arity}, _string, config) do
    cond do
      {name, arity} in @basic_types ->
        {:ok, Autolink.base_url(:ex_doc, :elixir, "typespecs", config) <> "#basic-types"}

      {name, arity} in @built_in_types ->
        {:ok, Autolink.base_url(:ex_doc, :elixir, "typespecs", config) <> "#built-in-types"}

      true ->
        :error
    end
  end

  defp try_autoimported(_ref, _string, _config) do
    :error
  end

  defp remove_prefix({:code, attrs, ["t:" <> rest], meta}), do: {:code, attrs, [rest], meta}
  defp remove_prefix({:code, attrs, ["c:" <> rest], meta}), do: {:code, attrs, [rest], meta}
  defp remove_prefix(other), do: other

  defp extras(href, config) do
    with %URI{scheme: nil, host: nil} = uri <- URI.parse(href),
         true <- is_binary(uri.path),
         ext when ext in [".md", ".txt", ""] <- Path.extname(uri.path) do
      file = Path.basename(uri.path)

      if file in config.extras do
        without_ext = trim_ext(file, ext)
        fragment = (uri.fragment && "#" <> uri.fragment) || ""
        url = HTML.text_to_id(without_ext) <> config.ext <> fragment
        {:ok, url}
      else
        Autolink.maybe_warn({:extras, href}, config)
        :error
      end
    else
      _ ->
        :error
    end
  end

  defp trim_ext(path, ""), do: path
  defp trim_ext(path, ext), do: String.trim_trailing(path, ext)

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

    Regex.replace(regex, string, fn _all, call_string, module_string, name_string, rest ->
      module = string_to_module(module_string)
      name = String.to_atom(name_string)
      arity = count_args(rest, 0, 0)
      original_text = call_string <> "()"

      ref =
        cond do
          module ->
            {:type, module, name, arity}

          # skip `@type %{required(...), optional(...), ...}`
          name in [:required, :optional] and arity == 1 ->
            nil

          true ->
            {:type, name, arity}
        end

      url =
        ref &&
          case Autolink.url(ref, original_text, :public_module, config) do
            {:ok, url} ->
              url

            :error ->
              case try_autoimported(ref, original_text, config) do
                {:ok, url} ->
                  url

                :error ->
                  nil
              end
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
end
