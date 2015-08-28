defmodule ExDoc.Formatter.HTML.Autolink do
  import ExDoc.Formatter.HTML.Templates, only: [h: 1, enc_h: 1]
  @moduledoc """
  Conveniences for autolinking locals, types and more.
  """

  @elixir_docs "http://elixir-lang.org/docs/stable/"
  @erlang_docs "http://www.erlang.org/doc/man/"

  @doc """
  Receives a list of module nodes and autolink all docs and typespecs.
  """
  def all(modules, extension \\ ".html") do
    aliases = Enum.map modules, &(&1.module)
    modules
    |> Enum.map(&Task.async(fn -> process_module(&1, modules, aliases, extension) end))
    |> Enum.map(&Task.await(&1, :infinity))
  end

  defp process_module(module, modules, aliases, extension) do
    module
    |> all_docs(modules, extension)
    |> all_typespecs(aliases)
  end

  defp module_to_string(module) do
    inspect module.module
  end

  defp all_docs(module, modules, extension) do
    locals = Enum.map(module.docs, &(doc_prefix(&1) <> &1.id)) ++
      Enum.map(module.typespecs, &("t:" <> &1.id))

    moduledoc =
      if module.moduledoc do
        module.moduledoc
        |> local_doc(locals)
        |> project_doc(modules, module.id, extension)
      end

    docs = for node <- module.docs do
      doc =
        if node.doc do
          node.doc
          |> local_doc(locals)
          |> project_doc(modules, module.id, extension)
        end
      %{node | doc: doc}
    end

    typedocs = for node <- module.typespecs do
      doc =
        if node.doc do
          node.doc
          |> local_doc(locals)
          |> project_doc(modules, module.id, extension)
        end
      %{node | doc: doc}
    end

    %{module | moduledoc: moduledoc, docs: docs, typespecs: typedocs}
  end

  defp all_typespecs(module, aliases) do
    locals = Enum.map module.typespecs, fn
      %ExDoc.TypeNode{name: name, arity: arity} -> { name, arity }
    end

    typespecs = for typespec <- module.typespecs do
      %{typespec | spec: typespec(typespec.spec, locals, aliases)}
    end

    docs = for node <- module.docs do
      %{node | specs: Enum.map(node.specs, &typespec(&1, locals, aliases))}
    end

    %{module | typespecs: typespecs, docs: docs}
  end

  @doc """
  Converts the given `ast` to string while linking the locals
  given by `typespecs` as HTML.
  """
  def typespec({:when, _, [{:::, _, [left, {:|, _, _} = center]}, right]} = ast, typespecs, aliases) do
    if short_typespec?(ast) do
      typespec_to_string(ast, typespecs, aliases)
    else
      typespec_to_string(left, typespecs, aliases) <>
      " ::\n  " <> typespec_with_new_line(center, typespecs, aliases) <>
      " when " <> String.slice(typespec_to_string(right, typespecs, aliases), 1..-2)
    end
  end

  def typespec({:::, _, [left, {:|, _, _} = center]} = ast, typespecs, aliases) do
    if short_typespec?(ast) do
      typespec_to_string(ast, typespecs, aliases)
    else
      typespec_to_string(left, typespecs, aliases) <>
      " ::\n  " <> typespec_with_new_line(center, typespecs, aliases)
    end
  end

  def typespec(other, typespecs, aliases) do
    typespec_to_string(other, typespecs, aliases)
  end

  defp typespec_with_new_line({:|, _, [left, right]}, typespecs, aliases) do
    typespec_to_string(left, typespecs, aliases) <>
      " |\n  " <> typespec_with_new_line(right, typespecs, aliases)
  end

  defp typespec_with_new_line(other, typespecs, aliases) do
    typespec_to_string(other, typespecs, aliases)
  end

  defp typespec_to_string(ast, typespecs, aliases, extension \\ ".html") do
    Macro.to_string(ast, fn
      {name, _, args}, string when is_atom(name) and is_list(args) ->
        string = strip_parens(string, args)
        arity = length(args)
        if { name, arity } in typespecs do
          n = enc_h("#{name}")
          ~s[<a href="#t:#{n}/#{arity}">#{h(string)}</a>]
        else
         string
        end
      {{ :., _, [alias, name] }, _, args}, string when is_atom(name) and is_list(args) ->
        string = strip_parens(string, args)
        alias = expand_alias(alias)
        if source = get_source(alias, aliases) do
          n = enc_h("#{name}")
          ~s[<a href="#{source}#{enc_h(inspect alias)}#{extension}#t:#{n}/#{length(args)}">#{h(string)}</a>]
        else
          string
        end
      _, string ->
        string
    end)
  end

  defp short_typespec?(ast) do
    byte_size(Macro.to_string(ast)) < 60
  end

  defp strip_parens(string, []) do
    if :binary.last(string) == ?) do
      :binary.part(string, 0, byte_size(string)-2)
    else
      string
    end
  end

  defp strip_parens(string, _), do: string

  defp expand_alias({:__aliases__, _, [h|t]}) when is_atom(h), do: Module.concat([h|t])
  defp expand_alias(atom) when is_atom(atom), do: atom
  defp expand_alias(_), do: nil

  defp get_source(alias, aliases) do
    cond do
      is_nil(alias) -> nil
      alias in aliases -> ""
      app = lib_dir_app(alias, elixir_lib_dir()) -> @elixir_docs <> app <> "/"
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
    fun_re = ~r{(([ct]:)?([a-z\d_!\\?>\\|=&<!~+\\.\\+*^@-]+)/\d+)} |> Regex.source
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
  def project_doc(bin, modules, module_id \\ nil, extension \\ ".html") when is_binary(bin) do
    project_funs  = for m <- modules, d <- m.docs, do: doc_prefix(d) <> m.id <> "." <> d.id
    project_types = for m <- modules, d <- m.typespecs, do: "t:" <> m.id <> "." <> d.id
    project_funs  = project_funs ++ project_types

    project_modules =
      modules
      |> Enum.map(&module_to_string/1)
      |> Enum.uniq()

    bin
    |> elixir_functions(project_funs, extension)
    |> elixir_modules(project_modules, module_id, extension)
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
  def elixir_functions(bin, project_funs, extension \\ ".html") when is_binary(bin) do
    module_re = ~r{(([A-Z][A-Za-z_\d]+)\.)+} |> Regex.source
    fun_re = ~r{([ct]:)?(#{module_re}([a-z\d_!\\?>\\|=&<!~+\\.\\+*^@-]+)/\d+)} |> Regex.source
    regex = ~r{(?<!\[)`\s*(#{fun_re})\s*`(?!\])}
    lib_dir = elixir_lib_dir()

    Regex.replace(regex, bin, fn all, match ->
      {prefix, module, function, arity} = split_function(match)

      cond do
        match in project_funs ->
          "[`#{module}.#{function}/#{arity}`](#{module}#{extension}##{prefix}#{enc_h function}/#{arity})"
        app = lib_dir_app("Elixir." <> module, lib_dir) ->
          "[`#{module}.#{function}/#{arity}`](#{@elixir_docs}#{app}/#{module}.html##{prefix}#{enc_h function}/#{arity})"
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
  def elixir_modules(bin, modules, module_id \\ nil, extension \\ ".html") when is_binary(bin) do
    regex = ~r{(?<!\[)`\s*(([A-Z][A-Za-z_\d]+\.?)+)\s*`(?!\])}
    lib_dir = elixir_lib_dir()

    Regex.replace(regex, bin, fn all, match ->
      cond do
        match == module_id ->
          "[`#{match}`](#{match}#{extension}#content)"
        match in modules ->
          "[`#{match}`](#{match}#{extension})"
        app = lib_dir_app("Elixir." <> match, lib_dir) ->
          "[`#{match}`](#{@elixir_docs}#{app}/#{match}.html)"
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
    regex = ~r{(?<!\[)`\s*:([a-z_]+\.[0-9a-zA-Z_!\\?]+/\d+)\s*`(?!\])}
    lib_dir = erlang_lib_dir()

    Regex.replace(regex, bin, fn all, match ->
      {_, module, function, arity} = split_function(match)
      if lib_dir_app(module, lib_dir) do
        "[`:#{match}`](#{@erlang_docs}#{module}.html##{function}-#{arity})"
      else
        all
      end
    end)
  end

  ## Helpers

  defp lib_dir_app(module, lib_dir) do
    case :code.where_is_file('#{module}.beam') do
      :non_existing ->
        nil
      path ->
        path = List.to_string(path)
        if String.starts_with?(path, lib_dir) do
          path
          |> Path.dirname()
          |> Path.dirname()
          |> Path.basename()
        end
    end
  end

  defp elixir_lib_dir do
    case Application.fetch_env(:ex_doc, :elixir_lib_dir) do
      {:ok, lib_dir} ->
        lib_dir
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
        Application.put_env(:ex_doc, :elixir_lib_dir, lib_dir)
        lib_dir
    end
  end

  defp erlang_lib_dir do
    Path.expand(:code.lib_dir)
  end
end
