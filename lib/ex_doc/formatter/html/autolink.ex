defmodule ExDoc.Formatter.HTML.Autolink do
  @moduledoc false
  import ExDoc.Formatter.HTML.Templates, only: [h: 1, enc_h: 1]

  @type language :: :elixir | :erlang | :markdown
  @type kind :: :function | :module | :mix_task
  @type link_type :: :normal | :custom

  @backtick_token "<B706848484895T>"
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
  def compile(modules, extension, config) do
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

    lib_dirs = config.deps ++ default_lib_dirs()

    %{
      aliases: aliases,
      docs_refs: docs_refs ++ types_refs,
      extension: extension,
      lib_dirs: lib_dirs,
      modules_refs: modules_refs,
      skip_undefined_reference_warnings_on: config.skip_undefined_reference_warnings_on
    }
  end

  @regexes [
    {:module, :elixir, :normal},
    {:module, :elixir, :custom},
    {:function, :elixir, :normal},
    {:function, :elixir, :custom},
    {:function, :erlang, :normal},
    {:function, :erlang, :custom},
    {:mix_task, :elixir, :normal}
  ]

  @doc """
  Autolinks any documentation in the project.

  This is the main API to autolink any project documentation.
  """
  def project_doc(nil, _id, _compiled), do: nil

  def project_doc(string, id, compiled) when is_binary(string) and is_map(compiled) do
    config =
      compiled
      |> Map.put(:id, id)
      |> Map.put_new(:module_id, nil)
      |> Map.put_new(:locals, [])

    string = preprocess(string)

    string =
      Enum.reduce(@regexes, string, fn {kind, language, link_type}, acc ->
        link(acc, language, kind, link_type, config)
      end)

    postprocess(string)
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

    compiled =
      compiled
      |> Map.put(:module_id, module.id)
      |> Map.put(:locals, funs ++ types)

    moduledoc = project_doc(module.doc, id(module, nil), compiled)

    docs =
      for node <- module.docs do
        doc = project_doc(node.doc, id(module, node), compiled)
        %{node | doc: doc}
      end

    typedocs =
      for node <- module.typespecs do
        doc = project_doc(node.doc, id(module, node), compiled)
        %{node | doc: doc}
      end

    %{module | doc: moduledoc, docs: docs, typespecs: typedocs}
  end

  defp id(%{id: id}, nil), do: id
  defp id(%{id: mod_id}, %ExDoc.FunctionNode{id: id, type: :callback}), do: "c:#{mod_id}.#{id}"
  defp id(%{id: mod_id}, %ExDoc.FunctionNode{id: id}), do: "#{mod_id}.#{id}"
  defp id(%{id: mod_id}, %ExDoc.TypeNode{id: id}), do: "t:#{mod_id}.#{id}"

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
  def typespec(ast, typespecs, aliases \\ [], lib_dirs \\ default_lib_dirs()) do
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

  # Helper function for autolinking functions and modules.
  #
  # It autolinks all links for a certain `language` and of a certain `kind`.
  #
  # `language` can be: `:elixir`, `:erlang` or `:markdown`.
  #
  # `kind` is either `:function`, `:module`, or `:mix_task`.
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
  defp link(string, language, kind, link_type, options) when is_map(options) do
    regex = re_kind_language_link_type(kind, language, link_type)
    replace_fun = replace_fun(kind, language, link_type, options)
    Regex.replace(regex, string, replace_fun)
  end

  defp preprocess(string) do
    regex = ~r{
        \[([^\]]*?`[^\]]*?)\]
        \(([^\)]*?)\)
      }x

    Regex.replace(regex, string, fn _all, text, link ->
      new_text = String.replace(text, :binary.compile_pattern("`"), @backtick_token)
      "[#{new_text}](#{link})"
    end)
  end

  defp postprocess(string) do
    String.replace(string, :binary.compile_pattern(@backtick_token), "`")
  end

  # The heart of the autolinking logic
  defp replace_fun(kind, :erlang, link_type, options) do
    lib_dirs = options[:lib_dirs] || default_lib_dirs(:erlang)

    fn all, text, match ->
      pmfa = {_prefix, module, function, arity} = split_match(kind, match)
      text = default_text(":", link_type, pmfa, text)

      if doc = module_docs(:erlang, module, lib_dirs) do
        case kind do
          :module ->
            "[#{text}](#{doc}#{module}.html)"

          :function ->
            "[#{text}](#{doc}#{module}.html##{function}-#{arity})"
        end
      else
        all
      end
    end
  end

  defp replace_fun(:module, :elixir, link_type, options) do
    extension = options[:extension] || ".html"
    lib_dirs = options[:lib_dirs] || default_lib_dirs(:elixir)
    module_id = options[:module_id] || nil
    modules_refs = options[:modules_refs] || []

    fn all, text, match ->
      pmfa = split_match(:module, match)
      text = default_text("", link_type, pmfa, text)

      cond do
        match == module_id ->
          "[#{text}](#content)"

        match in modules_refs ->
          "[#{text}](#{match}#{extension})"

        doc = module_docs(:elixir, match, lib_dirs) ->
          "[#{text}](#{doc}#{match}.html)"

        true ->
          all
      end
    end
  end

  defp replace_fun(:function, :elixir, link_type, options) do
    aliases = options[:aliases] || []
    docs_refs = options[:docs_refs] || []
    modules_refs = options[:modules_refs] || []
    extension = options[:extension] || ".html"
    lib_dirs = options[:lib_dirs] || default_lib_dirs(:elixir)
    locals = options[:locals] || []
    elixir_docs = get_elixir_docs(aliases, lib_dirs)
    id = options[:id]
    module_id = options[:module_id]
    skip_warnings_on = options[:skip_undefined_reference_warnings_on] || []

    fn all, text, match ->
      pmfa = {prefix, module, function, arity} = split_match(:function, match)
      text = default_text("", link_type, pmfa, text)

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
          "[#{text}](#{elixir_docs}Kernel.SpecialForms" <>
            "#{extension}##{prefix}#{enc_h(function)}/#{arity})"

        module in modules_refs ->
          if module_id not in skip_warnings_on and id not in skip_warnings_on do
            IO.warn(
              "documentation references #{match} but it doesn't exist " <>
                "or it's listed as @doc false (parsing #{id} docs)",
              []
            )
          end

          all

        doc = module_docs(:elixir, module, lib_dirs) ->
          "[#{text}](#{doc}#{module}.html##{prefix}#{enc_h(function)}/#{arity})"

        true ->
          all
      end
    end
  end

  defp replace_fun(:mix_task, :elixir, :normal, options) do
    extension = options[:extension] || ".html"
    lib_dirs = options[:lib_dirs] || default_lib_dirs(:elixir)
    module_id = options[:module_id] || nil
    modules_refs = options[:modules_refs] || []

    fn all, text, "mix " <> task_name ->
      match = task_module(task_name)

      cond do
        match == module_id ->
          "[#{text}](#content)"

        match in modules_refs ->
          "[#{text}](#{match}#{extension})"

        doc = module_docs(:elixir, match, lib_dirs) ->
          "[#{text}](#{doc}#{match}.html)"

        true ->
          all
      end
    end
  end

  defp task_module("help " <> task_name) do
    task_module(task_name)
  end

  defp task_module(task_name) do
    task_module =
      task_name
      |> String.split(".")
      |> Enum.map(&Macro.camelize/1)
      |> Enum.join(".")

    "Mix.Tasks." <> task_module
  end

  ## Helpers

  defp default_text(_, :custom, _, text),
    do: text

  defp default_text(_, _, {_, "", fun, arity}, _text),
    do: "`#{fun}/#{arity}`"

  defp default_text(prefix, _, {_, module, "", ""}, _text),
    do: "`#{prefix}#{module}`"

  defp default_text(prefix, _, {_, module, fun, arity}, _text),
    do: "`#{prefix}#{module}.#{fun}/#{arity}`"

  defp default_lib_dirs(),
    do: default_lib_dirs(:elixir) ++ default_lib_dirs(:erlang)

  defp default_lib_dirs(:elixir),
    do: elixir_lib_dirs() ++ hex_lib_dirs()

  defp default_lib_dirs(:erlang),
    do: erlang_lib_dirs()

  defp module_docs(:elixir, module, lib_dirs),
    do: lib_dirs_to_doc("Elixir." <> module, lib_dirs)

  defp module_docs(:erlang, module, lib_dirs),
    do: lib_dirs_to_doc(module, lib_dirs)

  @doc false
  defp split_match(:module, string), do: {"", string, "", ""}
  defp split_match(:function, string), do: split_function(string)

  defp split_function("c:" <> string) do
    {_, mod, fun, arity} = split_function(string)
    {"c:", mod, fun, arity}
  end

  defp split_function("t:" <> string) do
    {_, mod, fun, arity} = split_function(string)
    {"t:", mod, fun, arity}
  end

  defp split_function(":" <> string) do
    split_function(string)
  end

  defp split_function(string) do
    string
    |> String.split("/")
    |> split_function_list()
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
        path = path |> List.to_string() |> Path.expand()

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
        path -> path |> List.to_string() |> Path.expand()
      end

    if File.exists?(path) do
      elixir_root_lib =
        path
        |> Path.dirname()
        |> Path.dirname()
        |> Path.dirname()

      elixir_root_lib <> "/" <> app <> "/ebin"
    else
      # if beam file doesn't exists it's likely an escript
      Path.dirname(path)
    end
  end

  defp hex_lib_dirs() do
    if Application.spec(:hex, :vsn) do
      [{Application.app_dir(:hex, "ebin"), @elixir_docs <> "hex/"}]
    else
      # if Hex is not loaded it's likely an escript
      []
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

  @doc false
  def backtick_token(), do: @backtick_token

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
      [ct]:                            # c:, t:
    }x
  end

  defp re(:m, :elixir) do
    ~r{
      ( [A-Z]                             # start with uppercase letter
        [_a-zA-Z0-9]*\.?                  # followed by optional letter, number or underscore
      )+                                  # this pattern could be repeated
      (?<!\.)                             # it must not end with a "."
    }x
  end

  defp re(:m, :erlang) do
    ~r{
      :                                   # prefix
      [A-Za-z_]+                          # module_name
    }x
  end

  defp re(:f, :elixir) do
    ~r{
      ([a-z_][_a-zA-Z0-9]*[\\?\\!]?)      # regular function_name
      |                                   # OR
      [\{\}=&\\|\\.<>~*^@\\+\\%\\!-\/]+   # special_form
    }x
  end

  defp re(:f, :erlang) do
    ~r{
      ([a-z_][_a-zA-Z0-9]*[\\?\\!]?)
    }x
  end

  defp re(:fa, language) when language in [:elixir, :erlang] do
    ~r{
      (#{re_source(:f, language)})         # function_name
      /\d+                                 # /arity
    }x
  end

  defp re(:mfa, :elixir) do
    ~r{
      (#{re_source(:prefix)})?             # optional callback/type identifier or ":"
      (
        (#{re_source(:m)}\.)
        #{re_source(:fa)}
      )
    }x
  end

  defp re(:mfa, :erlang) do
    ~r{
      #{re_source(:m, :erlang)}            # module_name
      \.                                   # "."
      #{re_source(:fa, :erlang)}           # function_name/arity
    }x
  end

  defp re(:local, :elixir) do
    ~r{
      (#{re_source(:prefix)})?             # optional callback or type identifier
      #{re_source(:fa)}                    # function_name/arity
    }x
  end

  defp re_kind_language(:module, :elixir) do
    ~r{
      #{re_source(:m)}
    }x
  end

  defp re_kind_language(:module, :erlang) do
    ~r{
      #{re_source(:m, :erlang)}
    }x
  end

  defp re_kind_language(:function, :elixir) do
    ~r{
      (#{re_source(:local)}) | (#{re_source(:mfa)})
    }x
  end

  defp re_kind_language(:function, :erlang) do
    ~r{
      #{re_source(:mfa, :erlang)}
    }x
  end

  defp re_kind_language(:mix_task, :elixir) do
    ~r{
      mix\ (help\ )?([a-z][a-z0-9\._]*)
    }x
  end

  defp re_kind_language_link_type(kind, language, link_type) do
    source = Regex.source(re_kind_language(kind, language))

    case link_type do
      :normal ->
        # Capture 1 is ignored
        ~r{
          (?<!\]\()                            # it shouldn't be preceded by "]("
          (?<!``)
          (`\s*                                # leading backtick
          (#{source})                          # CAPTURE 2
          \s*`)                                # trailing backtick
          (?!`)
          # (?!\)\])                           # it shouldn't be followed by ")]"
        }x

      :custom ->
        ~r{
          \[(.*?)\]          # CAPTURE 1
          \(`(#{source})`\)  # CAPTURE 2
        }x
    end
  end
end
