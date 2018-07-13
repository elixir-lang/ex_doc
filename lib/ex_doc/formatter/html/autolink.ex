defmodule ExDoc.Formatter.HTML.Autolink do
  @moduledoc """
  Conveniences for autolinking.
  """

  import ExDoc.Formatter.HTML.Templates, only: [h: 1, enc_h: 1]

  @elixir_docs "https://hexdocs.pm/"
  @erlang_docs "http://www.erlang.org/doc/man/"
  @basic_types_page "typespecs.html#basic-types"
  @built_in_types_page "typespecs.html#built-in-types"

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
    integer: 0,
    float: 0,
    neg_integer: 0,
    non_neg_integer: 0,
    pos_integer: 0,
    list: 1,
    nonempty_list: 1,
    improper_list: 2,
    maybe_improper_list: 2
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

  kernel_exports = Kernel.__info__(:functions) ++ Kernel.__info__(:macros)
  special_form_exports = Kernel.SpecialForms.__info__(:macros)

  @basic_type_strings for {f, a} <- @basic_types, do: "t:#{f}/#{a}"
  @built_in_type_strings for {f, a} <- @built_in_types, do: "t:#{f}/#{a}"
  @kernel_function_strings for {f, a} <- kernel_exports, do: "#{f}/#{a}"
  @special_form_strings for {f, a} <- special_form_exports, do: "#{f}/#{a}"

  @doc """
  Compiles information used during autolinks.
  """
  def compile(modules, extension, extra_lib_dirs) do
    aliases = Enum.map(modules, & &1.module)
    modules_refs = Enum.map(aliases, &inspect/1)

    types_refs =
      for module <- modules,
          type <- module.typespecs,
          do: "t:" <> module.id <> "." <> type.id

    docs_refs =
      for module <- modules,
          doc <- module.docs,
          prefix = doc_prefix(doc),
          entry <- [doc.id | doc.defaults],
          do: prefix <> module.id <> "." <> entry

    %{
      aliases: aliases,
      docs_refs: docs_refs ++ types_refs,
      extension: extension,
      lib_dirs: extra_lib_dirs ++ elixir_lib_dirs() ++ erlang_lib_dirs(),
      modules_refs: modules_refs
    }
  end

  @doc """
  Autolinks any documentation in the project.

  This is the main API to autolink any project documentation.
  """
  def project_doc(bin, compiled) when is_binary(bin) do
    project_doc(bin, nil, [], compiled)
  end

  defp project_doc(nil, _module_id, _locals, _compiled) do
    nil
  end

  defp project_doc(bin, module_id, locals, compiled) when is_binary(bin) do
    %{
      aliases: aliases,
      docs_refs: docs_refs,
      extension: extension,
      lib_dirs: lib_dirs,
      modules_refs: modules_refs
    } = compiled

    bin
    |> locals(locals, aliases, extension, lib_dirs)
    |> elixir_functions(docs_refs, extension, lib_dirs)
    |> elixir_modules(modules_refs, module_id, extension, lib_dirs)
    |> erlang_functions()
  end

  @doc """
  Autolinks all modules nodes.

  This is the main API to autolink any modules nodes.
  """
  def all(modules, compiled) do
    opts = [timeout: :infinity]

    modules
    |> Task.async_stream(&(&1 |> all_docs(compiled) |> all_typespecs(compiled)), opts)
    |> Enum.map(&elem(&1, 1))
  end

  defp all_docs(module, compiled) do
    funs =
      for doc <- module.docs,
          prefix = doc_prefix(doc),
          entry <- [doc.id | doc.defaults],
          do: prefix <> entry

    types = Enum.map(module.typespecs, &("t:" <> &1.id))
    locals = funs ++ types

    moduledoc = project_doc(module.doc, module.id, locals, compiled)

    docs =
      for module_node <- module.docs do
        doc = project_doc(module_node.doc, module.id, locals, compiled)
        %{module_node | doc: doc}
      end

    typedocs =
      for module_node <- module.typespecs do
        doc = project_doc(module_node.doc, module.id, locals, compiled)
        %{module_node | doc: doc}
      end

    %{module | doc: moduledoc, docs: docs, typespecs: typedocs}
  end

  defp all_typespecs(module, compiled) do
    %{aliases: aliases, lib_dirs: lib_dirs} = compiled

    locals =
      Enum.map(module.typespecs, fn
        %ExDoc.TypeNode{name: name, arity: arity} -> {name, arity}
      end)

    typespecs =
      for typespec <- module.typespecs do
        %{typespec | spec: typespec(typespec.spec, locals, aliases, lib_dirs)}
      end

    docs =
      for module_node <- module.docs do
        %{
          module_node
          | specs: Enum.map(module_node.specs, &typespec(&1, locals, aliases, lib_dirs))
        }
      end

    %{module | typespecs: typespecs, docs: docs}
  end

  @doc """
  Helper function for autolinking typespecs.

  It converts the given `ast` to string while linking
  the locals given by `typespecs` as HTML.
  """
  def typespec(ast, typespecs, aliases \\ [], lib_dirs \\ elixir_lib_dirs() ++ erlang_lib_dirs()) do
    {formatted, placeholders} =
      format_and_extract_typespec_placeholders(ast, typespecs, aliases, lib_dirs)

    replace_placeholders(formatted, placeholders)
  end

  @doc false
  def format_and_extract_typespec_placeholders(ast, typespecs, aliases, lib_dirs) do
    ref = make_ref()
    elixir_docs = get_elixir_docs(aliases, lib_dirs)

    {formatted_ast, placeholders} =
      Macro.prewalk(ast, %{}, fn
        {:::, _, [{name, meta, args}, right]}, placeholders
        when is_atom(name) and is_list(args) ->
          {{:::, [], [{{ref, name}, meta, args}, right]}, placeholders}

        # Consume this form so that we don't autolink `foo` in `foo :: bar`
        {{^ref, name}, _, args}, placeholders when is_atom(name) and is_list(args) ->
          {{name, [], args}, placeholders}

        {name, _, args} = form, placeholders when is_atom(name) and is_list(args) ->
          arity = length(args)

          cond do
            {name, arity} in @basic_types ->
              url = elixir_docs <> @basic_types_page
              put_placeholder(form, url, placeholders)

            {name, arity} in @built_in_types ->
              url = elixir_docs <> @built_in_types_page
              put_placeholder(form, url, placeholders)

            {name, arity} in typespecs ->
              n = enc_h("#{name}")
              url = "#t:#{n}/#{arity}"
              put_placeholder(form, url, placeholders)

            true ->
              {form, placeholders}
          end

        {{:., _, [alias, name]}, _, args} = form, placeholders
        when is_atom(name) and is_list(args) ->
          alias = expand_alias(alias)

          if source = get_source(alias, aliases, lib_dirs) do
            url = type_remote_url(source, alias, name, args)
            put_placeholder(form, url, placeholders)
          else
            {form, placeholders}
          end

        form, placeholders ->
          {form, placeholders}
      end)

    {format_ast(formatted_ast), placeholders}
  end

  defp type_remote_url(@erlang_docs = source, module, name, _args) do
    module = enc_h("#{module}")
    name = enc_h("#{name}")
    "#{source}#{module}.html#type-#{name}"
  end

  defp type_remote_url(source, alias, name, args) do
    name = enc_h("#{name}")
    "#{source}#{enc_h(inspect(alias))}.html#t:#{name}/#{length(args)}"
  end

  defp typespec_string_to_link(string, url) do
    {string_to_link, _string_with_parens} = split_string_to_link(string)
    ~s[<a href="#{url}">#{h(string_to_link)}</a>]
  end

  defp put_placeholder(form, url, placeholders) do
    string = Macro.to_string(form)
    link = typespec_string_to_link(string, url)

    case Enum.find(placeholders, fn {_key, value} -> value == link end) do
      {placeholder, _} ->
        form = put_elem(form, 0, placeholder)
        {form, placeholders}

      nil ->
        count = map_size(placeholders) + 1
        placeholder = placeholder(string, count)
        form = put_elem(form, 0, placeholder)
        {form, Map.put(placeholders, placeholder, link)}
    end
  end

  defp placeholder(string, count) do
    [name | _] = String.split(string, "(", trim: true)
    name_size = String.length(name)
    int_size = count |> Integer.digits() |> length()
    underscores_size = 2
    pad = String.duplicate("p", max(name_size - int_size - underscores_size, 1))
    :"_#{pad}#{count}_"
  end

  defp replace_placeholders(string, placeholders) do
    Regex.replace(~r"_p+\d+_", string, &Map.fetch!(placeholders, String.to_atom(&1)))
  end

  defp format_ast(ast) do
    ast
    |> Macro.to_string()
    |> Code.format_string!(line_length: 80)
    |> IO.iodata_to_binary()
  end

  @doc """
  Helper function for autolinking locals.

  Create links to locally defined functions, specified in `locals`
  as a list of `fun/arity` strings.

  Ignores functions which are already wrapped in markdown url syntax,
  e.g. `[test/1](url)`. If the function doesn't touch the leading
  or trailing `]`, e.g. `[my link link/1 is here](url)`, the fun/arity
  will get translated to the new href of the function.
  """
  def locals(bin, locals, aliases \\ [], extension \\ ".html", lib_dirs \\ elixir_lib_dirs()) do
    fun_re =
      Regex.source(
        ~r{(([ct]:)?([a-z_]+[A-Za-z_\d]*[\\?\\!]?|[\{\}=&\\|\\.<>~*^@\\+\\%\\!-\/]+)/\d+)}
      )

    regex = ~r{(?<!\[)`\s*(#{fun_re})\s*`(?!\])}
    elixir_docs = get_elixir_docs(aliases, lib_dirs)

    Regex.replace(regex, bin, fn all, match ->
      {prefix, _, function, arity} = split_function(match)
      text = "`#{function}/#{arity}`"

      cond do
        match in locals ->
          "[#{text}](##{prefix}#{enc_h(function)}/#{arity})"

        match in @basic_type_strings ->
          "[#{text}](#{elixir_docs}#{@basic_types_page})"

        match in @built_in_type_strings ->
          "[#{text}](#{elixir_docs}#{@built_in_types_page})"

        match in @kernel_function_strings ->
          "[#{text}](#{elixir_docs}Kernel#{extension}##{prefix}#{enc_h(function)}/#{arity})"

        match in @special_form_strings ->
          "[#{text}](#{elixir_docs}Kernel.SpecialForms" <>
            "#{extension}##{prefix}#{enc_h(function)}/#{arity})"

        true ->
          all
      end
    end)
  end

  @doc """
  Helper function for autolinking elixir functions.

  Project functions are specified in `project_funs` as a list of
  `Module.fun/arity` tuples.

  Functions wrapped in markdown url syntax can link to other docs if
  the url is wrapped in backticks, otherwise the url is used as is.
  If the function doesn't touch the leading or trailing `]`, e.g.
  `[my link Module.link/1 is here](url)`, the Module.fun/arity
  will get translated to the new href of the function.
  """
  def elixir_functions(bin, project_funs, extension \\ ".html", lib_dirs \\ elixir_lib_dirs())
      when is_binary(bin) do
    bin
    |> replace_custom_links(project_funs, extension, lib_dirs)
    |> replace_normal_links(project_funs, extension, lib_dirs)
  end

  module_re = Regex.source(~r{(([A-Z][A-Za-z_\d]+)\.)+})

  fun_re =
    Regex.source(
      ~r{([ct]:)?((#{module_re})?(([a-z_]+[A-Za-z_\d]*[\\?\\!]?)|[\{\}=&\\|\\.<>~*^@\\+\\%\\!-\/]+)/\d+)}
    )

  @custom_re ~r{\[(.*?)\]\(`(#{fun_re})`\)}
  @normal_re ~r{(?<!\[)`\s*(#{fun_re})\s*`(?!\])}

  defp replace_custom_links(bin, project_funs, extension, lib_dirs) do
    Regex.replace(@custom_re, bin, fn all, text, match ->
      replacement(all, match, project_funs, extension, lib_dirs, text)
    end)
  end

  defp replace_normal_links(bin, project_funs, extension, lib_dirs) do
    Regex.replace(@normal_re, bin, fn all, match ->
      replacement(all, match, project_funs, extension, lib_dirs)
    end)
  end

  defp replacement(all, match, project_funs, extension, lib_dirs, text \\ nil) do
    {prefix, module, function, arity} = split_function(match)
    text = text || "`#{module}.#{function}/#{arity}`"

    aliases = []
    elixir_docs = get_elixir_docs(aliases, lib_dirs)

    cond do
      match in project_funs ->
        "[#{text}](#{module}#{extension}##{prefix}#{enc_h(function)}/#{arity})"

      match in @kernel_function_strings ->
        "[#{text}](#{elixir_docs}Kernel#{extension}##{prefix}#{enc_h(function)}/#{arity})"

      match in @special_form_strings ->
        "[#{text}](#{elixir_docs}Kernel.SpecialForms#{extension}##{prefix}#{enc_h(function)}/#{
          arity
        })"

      doc = lib_dirs_to_doc("Elixir." <> module, lib_dirs) ->
        "[#{text}](#{doc}#{module}.html##{prefix}#{enc_h(function)}/#{arity})"

      true ->
        all
    end
  end

  @doc """
  Helper function for autolinking elixir modules.

  Ignores modules which are already wrapped in markdown url syntax,
  e.g. `[Module](url)`. If the module name doesn't touch the leading
  or trailing `]`, e.g. `[my link Module is here](url)`, the Module
  will get translated to the new href of the module.
  """
  def elixir_modules(
        bin,
        modules,
        module_id \\ nil,
        extension \\ ".html",
        lib_dirs \\ elixir_lib_dirs()
      )
      when is_binary(bin) do
    regex = ~r{(?<!\[)(?<!``)`\s*(([A-Z][A-Za-z_\d]+\.?)+)\s*`(?!\])}

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

  defp split_function(bin) when is_binary(bin) do
    split_function(String.split(bin, "/"))
  end

  defp split_function([modules, arity]) do
    {mod, name} =
      modules
      # this handles the case of the ".." function
      |> String.replace(~r{([^\.])\.}, "\\1 ")
      |> String.split(" ")
      |> Enum.split(-1)

    {"", Enum.join(mod, "."), hd(name), arity}
  end

  # handles "/" function
  defp split_function([modules, "", arity]) do
    split_function([modules <> "/", arity])
  end

  @doc """
  Helper function for autolinking erlang functions.

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

  defp doc_prefix(%{type: c}) when c in [:callback, :macrocallback], do: "c:"
  defp doc_prefix(%{type: _}), do: ""

  defp lib_dirs_to_doc(module, lib_dirs) do
    case :code.where_is_file('#{module}.beam') do
      :non_existing ->
        nil

      path ->
        path = List.to_string(path)

        lib_dirs
        |> Enum.filter(fn {lib_dir, _} -> String.starts_with?(path, lib_dir) end)
        |> Enum.sort_by(fn {lib_dir, _} -> -byte_size(lib_dir) end)
        |> case do
          [{_, doc} | _] -> doc
          _ -> nil
        end
    end
  end

  defp elixir_lib_dirs do
    case Application.fetch_env(:ex_doc, :elixir_lib_dirs) do
      {:ok, lib_dirs} ->
        lib_dirs

      :error ->
        lib_dirs =
          for app <- ~w(elixir eex iex logger mix ex_unit) do
            {elixir_lib_dir(app), @elixir_docs <> app <> "/"}
          end

        Application.put_env(:ex_doc, :elixir_lib_dirs, lib_dirs)
        lib_dirs
    end
  end

  defp elixir_lib_dir(app) do
    path =
      case :code.where_is_file('Elixir.Kernel.beam') do
        :non_existing -> ""
        path -> List.to_string(path)
      end

    if File.exists?(path) do
      path =
        path
        |> Path.dirname()
        |> Path.dirname()
        |> Path.dirname()

      path <> "/" <> app <> "/ebin"
    else
      # if beam file doesn't exists it's likely an escript
      Path.dirname(path)
    end
  end

  defp erlang_lib_dirs do
    case Application.fetch_env(:ex_doc, :erlang_lib_dirs) do
      {:ok, lib_dirs} ->
        lib_dirs

      :error ->
        lib_dirs = [{Path.expand(:code.lib_dir()), @erlang_docs}]
        Application.put_env(:ex_doc, :erlang_lib_dirs, lib_dirs)
        lib_dirs
    end
  end

  defp split_string_to_link(string) do
    case :binary.split(string, "(") do
      [head, tail] -> {head, "(" <> tail}
      [head] -> {head, ""}
    end
  end

  defp expand_alias({:__aliases__, _, [h | t]}) when is_atom(h), do: Module.concat([h | t])
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

  defp get_elixir_docs(aliases, lib_dirs) do
    get_source(Kernel, aliases, lib_dirs)
  end
end
