defmodule ExDoc.HTMLFormatter.Autolink do
  @moduledoc """
  Conveniences for autolinking locals, types and more.
  """

  @elixir_docs "http://elixir-lang.org/docs/master/"

  @doc """
  Escape `'`, `"`, `&`, `<` and `>` in the string using HTML entities.
  This is only intended for use by the HTML formatter.
  """
  def escape_html(binary) do
    escape_map = [{ %r(&), "\\&amp;" }, { %r(<), "\\&lt;" }, { %r(>), "\\&gt;" }, { %r("), "\\&quot;" }]
    Enum.reduce escape_map, binary, fn({ re, escape }, acc) -> Regex.replace(re, acc, escape) end
  end

  @doc """
  Receives a list of module nodes and autolink all docs and typespecs.
  """
  def all(modules) do
    aliases = Enum.map modules, &(&1.module)
    project_funs = all_functions(modules)
    Enum.map modules, &(&1 |> local_docs() |> project_docs(project_funs) |> all_typespecs(aliases))
  end

  defp local_docs(ExDoc.ModuleNode[] = module) do
    locals    = Enum.map module.docs, &(&1.id)
    moduledoc = module.moduledoc && local_doc(module.moduledoc, locals)

    docs = lc node inlist module.docs do
      node.update_doc fn(doc) -> doc && local_doc(doc, locals) end
    end

    module.moduledoc(moduledoc).docs(docs)
  end

  defp project_docs(ExDoc.ModuleNode[] = module, project_funs) do
    moduledoc = module.moduledoc && project_doc(module.moduledoc, project_funs)

    docs = lc node inlist module.docs do
      node.update_doc fn(doc) -> doc && project_doc(doc, project_funs) end
    end

    module.moduledoc(moduledoc).docs(docs)
  end

  defp all_typespecs(ExDoc.ModuleNode[] = module, aliases) do
    locals = Enum.map module.typespecs, fn
      ExDoc.TypeNode[name: name, arity: arity] -> { name, arity }
    end

    typespecs = lc ExDoc.TypeNode[] = typespec inlist module.typespecs do
      typespec.update_spec &typespec(&1, locals, aliases)
    end

    docs = lc node inlist module.docs do
      node.update_specs fn(specs) ->
        Enum.map(specs, &typespec(&1, locals, aliases))
      end
    end

    module.typespecs(typespecs).docs(docs)
  end

  defp all_functions(modules) do
    Enum.flat_map modules, fn (mod) ->
      Enum.map mod.docs, fn (d) ->
        mod.id <> "." <> d.id
      end
    end
  end

  @doc """
  Converts the given `ast` to string while linking the locals
  given by `typespecs` as HTML.
  """
  def typespec(ast, typespecs, aliases) do
    Macro.to_string(ast, fn
      { name, _, args }, string when is_atom(name) and is_list(args) ->
        arity = length(args)
        if { name, arity } in typespecs do
          %b[<a href="#t:#{name}/#{arity}">#{string}</a>]
        else
          string
        end
      { { :., _, [alias, name] }, _, args }, string when is_atom(name) and is_list(args) ->
        alias = expand_alias(alias)
        if source = get_source(alias, aliases) do
          %b[<a href="#{source}#{inspect alias}.html#t:#{name}/#{length(args)}">#{string}</a>]
        else
          string
        end
      _, string ->
        string
    end)
  end

  defp expand_alias({ :__aliases__, _, [h|t] }) when is_atom(h), do: Module.concat([h|t])
  defp expand_alias(atom) when is_atom(atom), do: atom
  defp expand_alias(_), do: nil

  defp get_source(alias, aliases) do
    cond do
      nil?(alias) -> nil
      alias in aliases -> ""
      from_elixir?(alias) -> @elixir_docs
      true -> nil
    end
  end

  defp from_elixir?(alias) do
    :lists.prefix(elixir_ebin, alias_ebin(alias))
  end

  defp alias_ebin(alias) do
    case :code.where_is_file('#{alias}.beam') do
      :non_existing -> ''
      path -> path
    end
  end

  defp elixir_ebin do
    case :code.where_is_file('Elixir.Kernel.beam') do
      :non_existing -> [0]
      path -> path |> Path.dirname |> Path.dirname |> Path.dirname
    end
  end

  @doc """
  Create links to locally defined functions, specified in `locals`
  as a list of `fun/arity` strings.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[test/1](url)`. In case the function doesn't touch the leading
  or trailing `]`, e.g. `[my link link/1 is here](url)`, the fun/arity
  will get translated to the new href of the function.
  """
  def local_doc(bin, locals) when is_binary(bin) do
    Regex.scan(%r{(?<!\[)`\s*([a-z_!\\?]+/\d+)\s*`(?!\])}, bin)
    |> Enum.uniq
    |> List.flatten
    |> Enum.filter(&(&1 in locals))
    |> Enum.reduce(bin, fn (x, acc) ->
         escaped = Regex.escape(x)
         Regex.replace(%r/(?<!\[)`(\s*(#{escaped})\s*)`(?!\])/, acc, "[`\\1`](#\\2)")
       end)
  end

  @doc """
  Create links to functions defined in the project, specified in `project_funs`
  as a list of `Module.fun/arity` tuples.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[Module.test/1](url)`. In case the function doesn't touch the leading
  or trailing `]`, e.g. `[my link Module.link/1 is here](url)`, the Module.fun/arity
  will get translated to the new href of the function.
  """
  def project_doc(bin, project_funs) when is_binary(bin) do
    Regex.scan(%r{(?<!\[)`\s*((([A-Z][A-Za-z]+)\.)+[a-z_!\\?]+/\d+)\s*`(?!\])}, bin)
    |> Enum.uniq
    |> List.flatten
    |> Enum.filter(&(&1 in project_funs))
    |> Enum.reduce(bin, fn (x, acc) ->
         { mod_str, function_name, arity } = split_function(x)
         escaped = Regex.escape(x)
         Regex.replace(%r/(?<!\[)`(\s*#{escaped}\s*)`(?!\])/, acc, 
           "[`\\1`](#{mod_str}.html##{function_name}/#{arity})")
       end)
  end

  defp split_function(bin) do
    [modules, arity] = String.split(bin, "/")
    { mod, name } = modules |> String.split(".") |> Enum.split(-1)
    { Enum.join(mod, "."), hd(name), arity }
  end
end
