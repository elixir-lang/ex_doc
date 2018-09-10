defmodule ExDoc.Formatter.HTML.Autolink do
  @moduledoc """
  Conveniences for autolinking.
  """
  import ExDoc.Formatter.HTML.Templates, only: [h: 1, enc_h: 1]

  @type language :: :elixir | :erlang | :markdown
  @type kind :: :function | :module
  @type link_type :: :normal | :custom

  @backtick_replacement "<B706848484895T>"

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
  Compiles information used during autolinking.
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
    options =
      compiled
      |> Map.merge(%{
        module_id: module_id,
        locals: locals
      })
      |> Map.to_list()

    link_everything(bin, options)
  end

  @doc """
  Autolinks all module nodes.

  This is the main API to autolink any module nodes.
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

  @kinds [:module, :function]
  @languages [:elixir, :erlang]
  @link_types [:custom, :normal]

  @regexes (for link_type <- @link_types,
                language <- @languages,
                kind <- @kinds do
              %{
                kind: kind,
                language: language,
                link_type: link_type
              }
            end)

  @doc false
  def locals(string, locals, aliases \\ [], extension \\ ".html", lib_dirs \\ elixir_lib_dirs()) do
    options = [
      locals: locals,
      aliases: aliases,
      extension: extension,
      lib_dirs: lib_dirs
    ]

    link(string, :elixir, :function, options)
  end

  @doc false
  def elixir_modules(
        string,
        module_refs,
        module_id \\ nil,
        extension \\ ".html",
        lib_dirs \\ elixir_lib_dirs()
      )
      when is_binary(string) do
    options = [
      module_refs: module_refs,
      module_id: module_id,
      extension: extension,
      lib_dirs: lib_dirs
    ]

    link(string, :elixir, :module, options)
  end

  @doc false
  def elixir_functions(string, docs_refs, extension \\ ".html", lib_dirs \\ elixir_lib_dirs())
      when is_binary(string) do
    options = [
      docs_refs: docs_refs,
      extension: extension,
      lib_dirs: lib_dirs
    ]

    link(string, :elixir, :function, options)
  end

  @doc false
  def erlang_modules(string) when is_binary(string) do
    link(string, :erlang, :module, [])
  end

  @doc false
  def erlang_functions(string) when is_binary(string) do
    link(string, :erlang, :function, [])
  end

  defp replace_fun(language, kind, link_type, options) do
    case link_type do
      :normal ->
        fn all, match ->
          replacement(all, language, kind, match, options)
        end

      :custom ->
        fn all, text, match ->
          replacement(all, language, kind, match, text, options)
        end
    end
  end

  # Helper function for autolinking functions and modules.
  #
  # It autolinks all links for a certain `language` and of a certain `kind`.
  #
  # `language` can be: `:elixir`, `:erlang` or `:markdown`.
  #
  # `kind` is either `:function` or `:module`.
  #
  # It accepts a list of `options` used in the replacement functions.
  # - `:aliases
  # - `:docs_refs`
  # - `:extension` - Default value is `".html"`
  # - `:lib_dirs`
  # - `:locals` - A list of local functions
  # - `:module_id` - Module of the current doc. Default value is `nil`
  # - `:modules_refs` - List of modules available
  #
  # Internal options:
  # - `:preprocess?` - `true` or `false`. Do preprocessing and postprocessing, such as replacing backticks
  #                     with a token
  @doc false
  @spec link(String.t(), language, kind, keyword) :: String.t()
  def link(string, language, kind, options) do
    options = Keyword.put_new(options, :preprocess?, true)

    string =
      if options[:preprocess?] do
        preprocess(string)
      else
        string
      end

    string =
      string
      |> link(language, kind, :normal, options)
      |> link(language, kind, :custom, options)

    string =
      if options[:preprocess?] do
        postprocess(string)
      else
        string
      end

    string
  end

  defp link(string, language, kind, link_type, options) do
    regex = regex_link_type(language, kind, link_type)
    replace_fun = replace_fun(language, kind, link_type, options)

    Regex.replace(regex, string, replace_fun)
  end

  @doc false
  def link_everything(string, options) when is_list(options) do
    # disable preprocess every time we run link/4,
    # and transform string manually before and after Enum.reduce
    options = Keyword.put_new(options, :preprocess?, false)

    string = preprocess(string)

    string =
      Enum.reduce(@regexes, string, fn %{
                                         kind: kind,
                                         language: language,
                                         link_type: link_type
                                       },
                                       acc ->
        link(acc, language, kind, link_type, options)
      end)

    postprocess(string)
  end

  # Replaces all backticks inside the text of custom links with @backtick_replacement.
  defp preprocess(string) do
    regex = ~r{
        \[(.*?`.*?)\]
        \((.*?)\)
      }x

    Regex.replace(regex, string, fn _all, text, link ->
      new_text = String.replace(text, :binary.compile_pattern("`"), @backtick_replacement)
      "[#{new_text}](#{link})"
    end)
  end

  # Reverts the changes done by `preprocess/1`.
  defp postprocess(string) do
    String.replace(string, :binary.compile_pattern(@backtick_replacement), "`")
    # string
  end

  @doc false
  # The heart of the autolinking logic
  @spec replacement(String.t(), language, kind, String.t(), keyword) :: String.t()
  def replacement(string, language, kind, match, text \\ nil, options) do
    aliases = Keyword.get(options, :aliases, [])
    docs_refs = Keyword.get(options, :docs_refs, [])
    extension = Keyword.get(options, :extension, ".html")
    lib_dirs = Keyword.get(options, :lib_dirs, default_lib_dirs(language))
    locals = Keyword.get(options, :locals, [])
    module_id = Keyword.get(options, :module_id, nil)
    modules_refs = Keyword.get(options, :modules_refs, [])

    pmfa = {prefix, module, function, arity} = split_function(match)
    text = text || default_text(language, kind, match, pmfa)

    elixir_docs = get_elixir_docs(aliases, lib_dirs)

    case language do
      :erlang ->
        if doc = module_docs(:erlang, module, lib_dirs) do
          case kind do
            :module ->
              "[#{text}](#{doc}#{module}.html)"

            :function ->
              "[#{text}](#{doc}#{module}.html##{function}-#{arity})"
          end
        else
          string
        end

      :elixir ->
        case kind do
          :module ->
            cond do
              match == module_id ->
                "[`#{match}`](#{match}#{extension}#content)"

              match in modules_refs ->
                "[`#{match}`](#{match}#{extension})"

              doc = module_docs(:elixir, match, lib_dirs) ->
                "[`#{match}`](#{doc}#{match}.html)"

              true ->
                string
            end

          :function ->
            cond do
              match in locals ->
                "[#{text}](##{prefix}#{enc_h(function)}/#{arity})"

              match in docs_refs ->
                "[#{text}](#{module}#{extension}##{prefix}#{enc_h(function)}/#{arity})"

              match in @basic_type_strings ->
                "[#{text}](#{elixir_docs}#{@basic_types_page})"

              match in @built_in_type_strings ->
                "[#{text}](#{elixir_docs}#{@built_in_types_page})"

              match in @kernel_function_strings ->
                "[#{text}](#{elixir_docs}Kernel#{extension}##{prefix}#{enc_h(function)}/#{arity})"

              match in @special_form_strings ->
                "[#{text}](#{elixir_docs}Kernel.SpecialForms#{extension}##{prefix}#{
                  enc_h(function)
                }/#{arity})"

              doc = module_docs(:elixir, module, lib_dirs) ->
                "[#{text}](#{doc}#{module}.html##{prefix}#{enc_h(function)}/#{arity})"

              true ->
                string
            end
        end
    end
  end

  ## Helpers

  defp default_text(:erlang, _kind, match, {_prefix, _module, _function, _arity}),
    do: "`#{match}`"

  defp default_text(:elixir, _kind, _match, {_prefix, module, function, arity}) do
    if module == "" do
      # local
      "`#{function}/#{arity}`"
    else
      "`#{module}.#{function}/#{arity}`"
    end
  end

  defp default_lib_dirs(:elixir),
    do: elixir_lib_dirs()

  defp default_lib_dirs(:erlang),
    do: erlang_lib_dirs()

  defp module_docs(:elixir, module, lib_dirs),
    do: lib_dirs_to_doc("Elixir." <> module, lib_dirs)

  defp module_docs(:erlang, module, lib_dirs),
    do: lib_dirs_to_doc(module, lib_dirs)

  @doc false
  def split_function(string) when is_binary(string),
    do: split_function_string(string)

  defp split_function_string("c:" <> string) do
    {_, mod, fun, arity} = split_function(string)
    {"c:", mod, fun, arity}
  end

  defp split_function_string("t:" <> string) do
    {_, mod, fun, arity} = split_function_string(string)
    {"t:", mod, fun, arity}
  end

  defp split_function_string(":" <> string) do
    {_, mod, fun, arity} = split_function_string(string)
    {":", mod, fun, arity}
  end

  defp split_function_string(string) do
    string
    |> String.split("/")
    |> split_function_list()
  end

  # handles a single module
  defp split_function_list([module]) do
    {"", module, "", ""}
  end

  defp split_function_list([modules, arity]) do
    {mod, name} =
      modules
      # this handles the case of the ".." function
      |> String.replace(~r{([^\.])\.}, "\\1 ")
      |> String.split(" ")
      |> Enum.split(-1)

    {"", Enum.join(mod, "."), hd(name), arity}
  end

  # handles "/" function
  defp split_function_list([modules, "", arity]) when is_binary(modules) do
    split_function_list([modules <> "/", arity])
  end

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

  ## REGULAR EXPRESSION HELPERS

  # Returns a the string source of a regular expression,
  # given the `name` and `language`
  defp re_source(name, language \\ :elixir) do
    Regex.source(re(name, language))
  end

  # Returns a regular expression
  # given the `name` and `language`
  defp re(:prefix, :elixir) do
    ~r{
      [ct]:                                     # c:, t:
    }x
  end

  defp re(:m, :elixir) do
    ~r{
      ( [A-Z]                                 # start with uppercase letter
        [_a-zA-Z0-9]*\.?                      # followed by optional letter, number or underscore
      )+                                      # this pattern could be repeated
      (?<!\.)                                 # it must not end with a "."
    }x
  end

  defp re(:m, :erlang) do
    ~r{
      :                                       # prefix
      # TODO: revise the erlang rules for module names 
      [a-z_]+                                 # module_name
    }x
  end

  defp re(:f, :elixir) do
    ~r{
      ([a-z_][_a-zA-Z0-9]*[\\?\\!]?)          # regular function_name
      |                                       # OR
      [\{\}=&\\|\\.<>~*^@\\+\\%\\!-\/]+       # special_form
    }x
  end

  defp re(:f, :erlang) do
    ~r{
      # TODO: revise the erlang rules for function names
      [0-9a-zA-Z_!\\?]+
    }x
  end

  defp re(:fa, language) when language in [:elixir, :erlang] do
    ~r{
      (#{re_source(:f, language)})            # function_name
      /\d+                                    # /arity
    }x
  end

  defp re(:mfa, :elixir) do
    ~r{
      (#{re_source(:prefix)})?                # optional callback/type identifier or ":"
      (
        (#{re_source(:m)}\.)
        #{re_source(:fa)}
      )
    }x
  end

  defp re(:mfa, :erlang) do
    ~r{
      #{re_source(:m, :erlang)}               # module_name
      \.                                      # "."
      #{re_source(:fa, :erlang)}              # function_name/arity
    }x
  end

  defp re(:local, :elixir) do
    ~r{
      (#{re_source(:prefix)})?               # optional callback or type identifier
      #{re_source(:fa)}                      # function_name/arity
    }x
  end

  defp re(:modules, :elixir) do
    ~r{
      #{re_source(:m)}
    }x
  end

  defp re(:modules, :erlang) do
    ~r{
      #{re_source(:m, :erlang)}
    }x
  end

  defp re(:functions, :elixir) do
    ~r{
      (#{re_source(:local)}) | (#{re_source(:mfa)})
    }x
  end

  defp re(:functions, :erlang) do
    ~r{
      #{re_source(:mfa, :erlang)}
    }x
  end

  defp re({:normal_link, function_re_source}, :markdown) do
    ~r{
      (?<!\]\()                                 # it shouldn't be preceded by "]("
      (?<!``)
      `\s*                                      # leading backtick
      (#{function_re_source})                   # CAPTURE 1
      \s*`                                      # trailing backtick
      (?!`)
      # (?!\)\])                                # it shouldn't be followed by ")]"
    }x
  end

  defp re({:custom_link, function_re_source}, :markdown) do
    re({:custom_link, "(.*?)", function_re_source}, :markdown)
  end

  defp re({:custom_link, text_re_source, function_re_source}, :markdown) do
    ~r{
      \[#{text_re_source}\]                     # CAPTURE 1
      \(`(#{function_re_source})`\)             # CAPTURE 2
    }x
  end

  defp regex_link_type(language, kind, link_type) do
    group =
      case kind do
        :function -> :functions
        :module -> :modules
      end

    case link_type do
      :normal ->
        re({:normal_link, re_source(group, language)}, :markdown)

      :custom ->
        re({:custom_link, re_source(group, language)}, :markdown)
    end
  end
end
