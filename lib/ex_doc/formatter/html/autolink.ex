defmodule ExDoc.Formatter.HTML.Autolink do
  @moduledoc """
  Conveniences for autolinking locals, types and more.
  """

  import ExDoc.Formatter.HTML.Templates, only: [h: 1, enc_h: 1]

  @elixir_docs "https://hexdocs.pm/"
  @erlang_docs "http://www.erlang.org/doc/man/"

  @doc """
  Receives a list of module nodes and autolink all docs and typespecs.
  """
  def all(modules, extension, extra_lib_dirs) do
    aliases = Enum.map modules, &(&1.module)
    lib_dirs = extra_lib_dirs ++ elixir_lib_dirs()
    modules
    |> Enum.map(&Task.async(fn -> process_module(&1, modules, aliases, extension, lib_dirs) end))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp process_module(module, modules, aliases, extension, lib_dirs) do
    module
    |> all_docs(modules, extension, lib_dirs)
    |> all_typespecs(aliases, lib_dirs)
  end

  defp module_to_string(module) do
    inspect module.module
  end

  defp all_docs(module, modules, extension, lib_dirs) do
    locals =
      for doc <- module.docs,
          prefix = doc_prefix(doc),
          entry <- [doc.id | doc.defaults],
          do: prefix <> entry,
          into: Enum.map(module.typespecs, &("t:" <> &1.id))

    moduledoc =
      if module.doc do
        module.doc
        |> local_doc(locals)
        |> project_doc(modules, module.id, extension, lib_dirs)
      end

    docs = for module_node <- module.docs do
      doc =
        if module_node.doc do
          module_node.doc
          |> local_doc(locals)
          |> project_doc(modules, module.id, extension, lib_dirs)
        end
      %{module_node | doc: doc}
    end

    typedocs = for module_node <- module.typespecs do
      doc =
        if module_node.doc do
          module_node.doc
          |> local_doc(locals)
          |> project_doc(modules, module.id, extension, lib_dirs)
        end
      %{module_node | doc: doc}
    end

    %{module | doc: moduledoc, docs: docs, typespecs: typedocs}
  end

  defp all_typespecs(module, aliases, lib_dirs) do
    locals = Enum.map module.typespecs, fn
      %ExDoc.TypeNode{name: name, arity: arity} -> {name, arity}
    end

    typespecs = for typespec <- module.typespecs do
      %{typespec | spec: typespec(typespec.spec, locals, aliases, lib_dirs)}
    end

    docs = for module_node <- module.docs do
      %{module_node | specs: Enum.map(module_node.specs, &typespec(&1, locals, aliases, lib_dirs))}
    end

    %{module | typespecs: typespecs, docs: docs}
  end

  @doc """
  Converts the given `ast` to string while linking the locals
  given by `typespecs` as HTML.
  """
  def typespec(ast, typespecs, aliases, lib_dirs \\ elixir_lib_dirs())

  def typespec({:when, _, [{:::, _, [left, {:|, _, _} = center]}, right]} = ast, typespecs, aliases, lib_dirs) do
    if short_typespec?(ast) do
      normalize_left(ast, typespecs, aliases, lib_dirs)
    else
      normalize_left(left, typespecs, aliases, lib_dirs) <>
      " ::\n  " <> typespec_with_new_line(center, typespecs, aliases, lib_dirs) <>
      " when " <> String.slice(typespec_to_string(right, typespecs, aliases, lib_dirs), 1..-2)
    end
  end

  def typespec({:::, _, [left, {:|, _, _} = center]} = ast, typespecs, aliases, lib_dirs) do
    if short_typespec?(ast) do
      normalize_left(ast, typespecs, aliases, lib_dirs)
    else
      normalize_left(left, typespecs, aliases, lib_dirs) <>
      " ::\n  " <> typespec_with_new_line(center, typespecs, aliases, lib_dirs)
    end
  end

  def typespec(other, typespecs, aliases, lib_dirs) do
    normalize_left(other, typespecs, aliases, lib_dirs)
  end

  defp typespec_with_new_line({:|, _, [left, right]}, typespecs, aliases, lib_dirs) do
    typespec_to_string(left, typespecs, aliases, lib_dirs) <>
      " |\n  " <> typespec_with_new_line(right, typespecs, aliases, lib_dirs)
  end

  defp typespec_with_new_line(other, typespecs, aliases, lib_dirs) do
    typespec_to_string(other, typespecs, aliases, lib_dirs)
  end

  defp normalize_left({:::, _, [{name, meta, args}, right]}, typespecs, aliases, lib_dirs) do
    new_args =
      Enum.map(args, &[self(), typespec_to_string(&1, typespecs, aliases, lib_dirs)])
    new_left =
      Macro.to_string {name, meta, new_args}, fn
        [pid, string], _ when pid == self() -> string
        _, string -> string
      end
    new_left <> " :: " <> typespec_to_string(right, typespecs, aliases, lib_dirs)
  end

  defp normalize_left({:when, _, [{:::, _, _} = left, right]}, typespecs, aliases, lib_dirs) do
    normalize_left(left, typespecs, aliases, lib_dirs) <>
    " when " <> String.slice(typespec_to_string(right, typespecs, aliases, lib_dirs), 1..-2)
  end

  defp normalize_left(ast, typespecs, aliases, lib_dirs) do
    typespec_to_string(ast, typespecs, aliases, lib_dirs)
  end

  defp typespec_to_string(ast, typespecs, aliases, lib_dirs) do
    Macro.to_string(ast, fn
      {name, _, args}, string when is_atom(name) and is_list(args) ->
        string = strip_parens(string, args)
        arity = length(args)
        if {name, arity} in typespecs do
          n = enc_h("#{name}")
          {string_to_link, string_with_parens} = split_string_to_link(string)
          ~s[<a href="#t:#{n}/#{arity}">#{h(string_to_link)}</a>#{string_with_parens}]
        else
          string
        end
      {{:., _, [alias, name]}, _, args}, string when is_atom(name) and is_list(args) ->
        string = strip_parens(string, args)
        alias = expand_alias(alias)
        if source = get_source(alias, aliases, lib_dirs) do
          n = enc_h("#{name}")
          {string_to_link, string_with_parens} = split_string_to_link(string)
          ~s[<a href="#{source}#{enc_h(inspect alias)}.html#t:#{n}/#{length(args)}">#{h(string_to_link)}</a>#{string_with_parens}]
        else
          string
        end
      _, string ->
        string
    end)
  end

  defp short_typespec?(ast) do
    byte_size(Macro.to_string(ast)) <= 70
  end

  defp strip_parens(string, []) do
    if :binary.last(string) == ?) do
      :binary.part(string, 0, byte_size(string) - 2)
    else
      string
    end
  end
  defp strip_parens(string, _), do: string

  defp split_string_to_link(string) do
    case :binary.split(string, "(") do
      [head, tail] -> {head, "(" <> tail}
      [head] -> {head, ""}
    end
  end

  defp expand_alias({:__aliases__, _, [h|t]}) when is_atom(h), do: Module.concat([h|t])
  defp expand_alias(atom) when is_atom(atom), do: atom
  defp expand_alias(_), do: nil

  defp get_source(alias, aliases, lib_dirs) do
    cond do
      is_nil(alias) -> nil
      alias in aliases -> ""
      doc = lib_dirs_to_doc(alias, lib_dirs) -> doc
      true -> nil
    end
  end

  @doc """
  Create links to locally defined functions, specified in `locals`
  as a list of `fun/arity` strings.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[test/1](url)`. If the function doesn't touch the leading
  or trailing `]`, e.g. `[my link link/1 is here](url)`, the fun/arity
  will get translated to the new href of the function.
  """
  def local_doc(bin, locals) when is_binary(bin) do
    fun_re = Regex.source(~r{(([ct]:)?([a-z_]+[A-Za-z_\d]*[\\?\\!]?|[\{\}=&\\|\\.<>~*^@\\+\\%\\!-]+)/\d+)})
    regex = ~r{(?<!\[)`\s*(#{fun_re})\s*`(?!\])}
    Regex.replace(regex, bin, fn all, match ->
      if match in locals do
        {prefix, _, function, arity} = split_function(match)
        "[`#{function}/#{arity}`](##{prefix}#{enc_h function}/#{arity})"
      else
        all
      end
    end)
  end

  @doc """
  Creates links to modules and functions defined in the project.
  """
  def project_doc(bin, modules, module_id \\ nil,
                  extension \\ ".html", lib_dirs \\ elixir_lib_dirs()) when is_binary(bin) do
    project_types =
      for module <- modules,
          type <- module.typespecs,
          do: "t:" <> module.id <> "." <> type.id

    project_docs =
      for module <- modules,
          doc <- module.docs,
          prefix = doc_prefix(doc),
          entry <- [doc.id | doc.defaults],
          do: prefix <>  module.id <> "." <> entry,
          into: project_types

    project_modules =
      modules
      |> Enum.map(&module_to_string/1)
      |> Enum.uniq()

    bin
    |> elixir_functions(project_docs, extension, lib_dirs)
    |> elixir_modules(project_modules, module_id, extension, lib_dirs)
    |> erlang_functions()
  end

  defp doc_prefix(%{type: c}) when c in [:callback, :macrocallback], do: "c:"
  defp doc_prefix(%{type: _}), do: ""

  @doc """
  Create links to elixir functions defined in the project and Elixir itself.

  Project functions are specified in `project_funs` as a list of
  `Module.fun/arity` tuples.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[Module.test/1](url)`. If the function doesn't touch the leading
  or trailing `]`, e.g. `[my link Module.link/1 is here](url)`, the Module.fun/arity
  will get translated to the new href of the function.
  """
  def elixir_functions(bin, project_funs, extension \\ ".html", lib_dirs \\ elixir_lib_dirs()) when is_binary(bin) do
    module_re = Regex.source(~r{(([A-Z][A-Za-z_\d]+)\.)+})
    fun_re = Regex.source(~r{([ct]:)?(#{module_re}([a-z_]+[A-Za-z_\d]*[\\?\\!]?|[\{\}=&\\|\\.<>~*^@\\+\\%\\!-]+)/\d+)})
    regex = ~r{(?<!\[)`\s*(#{fun_re})\s*`(?!\])}

    Regex.replace(regex, bin, fn all, match ->
      {prefix, module, function, arity} = split_function(match)

      cond do
        match in project_funs ->
          "[`#{module}.#{function}/#{arity}`](#{module}#{extension}##{prefix}#{enc_h function}/#{arity})"
        doc = lib_dirs_to_doc("Elixir." <> module, lib_dirs) ->
          "[`#{module}.#{function}/#{arity}`](#{doc}#{module}.html##{prefix}#{enc_h function}/#{arity})"
        true ->
          all
      end
    end)
  end

  @doc """
  Create links to elixir modules defined in the project and
  in Elixir itself.

  Ignores modules which are already wrapped in markdown url syntax,
  e.g. `[Module](url)`. If the module name doesn't touch the leading
  or trailing `]`, e.g. `[my link Module is here](url)`, the Module
  will get translated to the new href of the module.
  """
  def elixir_modules(bin, modules, module_id \\ nil,
                     extension \\ ".html", lib_dirs \\ elixir_lib_dirs()) when is_binary(bin) do
    regex = ~r{(?<!\[)`\s*(([A-Z][A-Za-z_\d]+\.?)+)\s*`(?!\])}

    Regex.replace(regex, bin, fn all, match ->
      cond do
        match == module_id ->
          "[`#{match}`](#{match}#{extension}#content)"
        match in modules ->
          "[`#{match}`](#{match}#{extension})"
        doc = lib_dirs_to_doc("Elixir." <> match, lib_dirs) ->
          "[`#{match}`](#{doc}#{match}.html)"
        true ->
          all
      end
    end)
  end

  defp split_function("c:" <> bin) do
    {_, mod, fun, arity} = split_function(bin)
    {"c:", mod, fun, arity}
  end
  defp split_function("t:" <> bin) do
    {_, mod, fun, arity} = split_function(bin)
    {"t:", mod, fun, arity}
  end
  defp split_function(bin) do
    [modules, arity] = String.split(bin, "/")
    {mod, name} =
      modules
      |> String.replace(~r{([^\.])\.}, "\\1 ") # this handles the case of the ".." function
      |> String.split(" ")
      |> Enum.split(-1)
    {"", Enum.join(mod, "."), hd(name), arity}
  end

  @doc """
  Create links to Erlang functions in code blocks.

  Only links modules that are in the Erlang distribution `lib_dir`
  and only link functions in those modules that export a function of the
  same name and arity.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[:module.test/1](url)`. If the function doesn't touch the leading
  or trailing `]`, e.g. `[my link :module.link/1 is here](url)`, the :module.fun/arity
  will get translated to the new href of the function.
  """
  def erlang_functions(bin) when is_binary(bin) do
    lib_dirs = erlang_lib_dirs()
    regex = ~r{(?<!\[)`\s*:([a-z_]+\.[0-9a-zA-Z_!\\?]+/\d+)\s*`(?!\])}
    Regex.replace(regex, bin, fn all, match ->
      {_, module, function, arity} = split_function(match)
      if doc = lib_dirs_to_doc(module, lib_dirs) do
        "[`:#{match}`](#{doc}#{module}.html##{function}-#{arity})"
      else
        all
      end
    end)
  end

  ## Helpers

  defp lib_dirs_to_doc(module, lib_dirs) do
    case :code.where_is_file('#{module}.beam') do
      :non_existing ->
        nil
      path ->
        path = List.to_string(path)
        Enum.find_value(lib_dirs, fn {lib_dir, doc} ->
          String.starts_with?(path, lib_dir) and doc
        end)
    end
  end

  defp elixir_lib_dirs do
    case Application.fetch_env(:ex_doc, :elixir_lib_dirs) do
      {:ok, lib_dirs} ->
        lib_dirs
      :error ->
        lib_dir =
          case :code.where_is_file('Elixir.Kernel.beam') do
            :non_existing ->
              [0]
            path ->
              path
              |> Path.dirname()
              |> Path.dirname()
              |> Path.dirname()
          end

        lib_dirs =
          for app <- ~w(elixir eex iex logger mix ex_unit) do
            {lib_dir <> "/" <> app <> "/ebin", @elixir_docs <> app <> "/"}
          end
        Application.put_env(:ex_doc, :elixir_lib_dirs, lib_dirs)
        lib_dirs
    end
  end

  defp erlang_lib_dirs do
    case Application.fetch_env(:ex_doc, :erlang_lib_dirs) do
      {:ok, lib_dirs} ->
        lib_dirs
      :error ->
        lib_dirs = [{Path.expand(:code.lib_dir), @erlang_docs}]
        Application.put_env(:ex_doc, :erlang_lib_dirs, lib_dirs)
        lib_dirs
    end
  end
end
