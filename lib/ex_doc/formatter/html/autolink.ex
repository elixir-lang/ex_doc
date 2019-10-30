defmodule ExDoc.Formatter.HTML.Autolink do
  @moduledoc false
  import ExDoc.Formatter.HTML.Templates, only: [h: 1, enc: 1]

  @type language :: :elixir | :erlang
  @type kind :: :function | :module | :mix_task
  @type link_type :: :normal | :custom

  @backtick_token "<B706848484895T>"
  @elixir_docs "https://hexdocs.pm/"
  @erlang_docs "http://www.erlang.org/doc/man/"

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
    %{aliases: aliases, lib_dirs: lib_dirs, extension: extension} = compiled

    locals =
      Enum.map(module.typespecs, fn
        %ExDoc.TypeNode{name: name, arity: arity} -> {name, arity}
      end)

    typespecs =
      for typespec <- module.typespecs do
        spec = typespec(typespec.name, typespec.spec, locals, aliases, lib_dirs, extension)
        %{typespec | spec: spec}
      end

    docs =
      for node <- module.docs do
        specs =
          Enum.map(node.specs, &typespec(node.name, &1, locals, aliases, lib_dirs, extension))

        %{node | specs: specs}
      end

    %{module | typespecs: typespecs, docs: docs}
  end

  @doc """
  Helper function for autolinking typespecs.

  It converts the given `ast` to string while linking
  the locals given by `typespecs` as HTML.
  """
  def typespec(
        name,
        ast,
        typespecs,
        aliases \\ [],
        lib_dirs \\ default_lib_dirs(),
        extension \\ ".html"
      ) do
    string =
      ast
      |> Macro.to_string()
      |> Code.format_string!(line_length: 80)
      |> IO.iodata_to_binary()

    name = Atom.to_string(name)
    {name, rest} = split_name(string, name)
    name <> do_typespec(rest, typespecs, aliases, lib_dirs, extension)
  end

  # extract out function name so we don't process it. This is to avoid linking it when there's
  # a type with the same name
  defp split_name(string, name) do
    if String.starts_with?(string, name) do
      {name, binary_part(string, byte_size(name), byte_size(string) - byte_size(name))}
    else
      {"", string}
    end
  end

  def do_typespec(string, typespecs, aliases, lib_dirs, extension) do
    regex =
      ~r/((?:((?:\:[a-z][_a-zA-Z0-9]*)|(?:[A-Z][_a-zA-Z0-9]*(?:\.[A-Z][_a-zA-Z0-9]*)*))\.)?(\w+))(\(.*\))/

    Regex.replace(regex, string, fn _all, call_string, module_string, name_string, rest ->
      name = String.to_atom(name_string)
      arity = count_args(rest, 0, 0)
      module = if module_string != "", do: module_string |> module()
      url = url(module, name, arity, typespecs, aliases, lib_dirs, extension)

      if url do
        ~s[<a href="#{url}">#{h(call_string)}</a>]
      else
        call_string
      end <> do_typespec(rest, typespecs, aliases, lib_dirs, extension)
    end)
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

  defp module(string) do
    if String.starts_with?(string, ":") do
      string |> String.trim_leading(":") |> String.to_atom()
    else
      Module.concat([string])
    end
  end

  # local
  defp url(nil, name, arity, typespecs, aliases, lib_dirs, extension) do
    cond do
      {name, arity} in typespecs ->
        "#t:#{name}/#{arity}"

      {name, arity} in @basic_types ->
        elixir_docs = get_elixir_docs(aliases, lib_dirs)
        basic_types_page_for(elixir_docs, extension)

      {name, arity} in @built_in_types ->
        elixir_docs = get_elixir_docs(aliases, lib_dirs)
        built_in_types_page_for(elixir_docs, extension)

      true ->
        nil
    end
  end

  # remote
  defp url(module, name, arity, _typespecs, aliases, lib_dirs, extension) do
    cond do
      module in aliases ->
        external_url("", module, name, arity, extension)

      source = get_source(module, aliases, lib_dirs) ->
        external_url(source, module, name, arity, extension)

      true ->
        nil
    end
  end

  defp external_url(@erlang_docs, module, name, _arity, _extension) do
    module = enc("#{module}")
    name = enc("#{name}")
    "#{@erlang_docs}#{module}.html#type-#{name}"
  end

  defp external_url(source, module, name, arity, extension) do
    name = enc("#{name}")
    "#{source}#{enc(inspect(module))}#{extension}#t:#{name}/#{arity}"
  end

  # Helper function for autolinking functions and modules.
  #
  # It autolinks all links for a certain `language` and of a certain `kind`.
  #
  # `language` can be: `:elixir`, `:erlang`.
  #
  # `kind` is either `:function`, `:module`, or `:mix_task`.
  #
  # It accepts a list of `options` used in the replacement functions.
  #
  #   * `:aliases
  #   * `:docs_refs`
  #   * `:extension` - Default value is `".html"`
  #   * `:lib_dirs`
  #   * `:locals` - A list of local functions
  #   * `:module_id` - Module of the current doc. Default value is `nil`
  #   * `:modules_refs` - List of modules available
  #
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

  # TODO: We only need this clause because Erlang is not hosted
  # on Hexdocs. If we can successfully migrate Erlang there, we
  # can unify all of this.
  defp replace_fun(kind, :erlang, link_type, options) do
    lib_dirs = options[:lib_dirs] || default_lib_dirs(:erlang)

    fn all, text, full_module, module, separator, function, comma_arities ->
      text =
        if link_type == :custom do
          text
        else
          "`#{full_module}#{separator}#{function}/#{comma_arities}`"
        end

      arities = split_arities(comma_arities)

      if doc = module_docs(:erlang, module, lib_dirs) do
        case kind do
          :module ->
            "[#{text}](#{doc}#{module}.html)"

          :function ->
            "[#{text}](#{doc}#{module}.html##{function}-#{hd(arities)})"
        end
      else
        all
      end
    end
  end

  # This is the code we need to make language agnostic for auto-linking to work everywhere
  defp replace_fun(:module, :elixir, link_type, options) do
    extension = options[:extension] || ".html"
    lib_dirs = options[:lib_dirs] || default_lib_dirs(:elixir)
    module_id = options[:module_id] || nil
    modules_refs = options[:modules_refs] || []

    fn all, text, full_module, module ->
      text = if link_type == :custom, do: text, else: "`#{full_module}`"

      cond do
        module == module_id ->
          "[#{text}](#content)"

        module in modules_refs ->
          "[#{text}](#{module}#{extension})"

        doc = module_docs(:elixir, module, lib_dirs) ->
          "[#{text}](#{doc}#{module}#{extension})"

        true ->
          all
      end
    end
  end

  # This is the other half of the code we need to make language agnostic for auto-linking to work
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

    fn all, text, prefix, full_module, module, separator, function, comma_arities ->
      text =
        if link_type == :custom do
          text
        else
          "`#{full_module}#{separator}#{function}/#{comma_arities}`"
        end

      arities = split_arities(comma_arities)
      first_arity = hd(arities)
      multiple_arities? = Enum.count(arities) > 1

      pmf = prefix <> module <> separator <> function
      pmfa = pmf <> "/" <> comma_arities

      cond do
        multiple_arities? and not arities_sorted?(arities) ->
          all

        Enum.all?(arities, fn a -> "#{pmf}/#{a}" in locals end) ->
          "[#{text}](##{prefix}#{enc(function)}/#{first_arity})"

        Enum.all?(arities, fn a -> "#{pmf}/#{a}" in docs_refs end) ->
          "[#{text}](#{module}#{extension}##{prefix}#{enc(function)}/#{first_arity})"

        pmfa in locals ->
          "[#{text}](##{prefix}#{enc(function)}/#{first_arity})"

        Enum.all?(arities, fn a -> "#{pmf}/#{a}" in @kernel_function_strings end) ->
          "[#{text}](#{elixir_docs}Kernel#{extension}##{prefix}#{enc(function)}/#{first_arity})"

        Enum.all?(arities, fn a -> "#{pmf}/#{a}" in @special_form_strings end) ->
          "[#{text}](#{elixir_docs}Kernel.SpecialForms#{extension}" <>
            "##{prefix}#{enc(function)}/#{first_arity})"

        pmfa in @basic_type_strings ->
          "[#{text}](#{basic_types_page_for(elixir_docs, extension)})"

        pmfa in @built_in_type_strings ->
          "[#{text}](#{built_in_types_page_for(elixir_docs, extension)})"

        module in modules_refs ->
          maybe_warn(text, pmfa, module_id, id, skip_warnings_on)

        doc = module_docs(:elixir, module, lib_dirs) ->
          "[#{text}](#{doc}#{module}#{extension}##{prefix}#{enc(function)}/#{first_arity})"

        link_type == :custom ->
          maybe_warn(text, pmfa, module_id, id, skip_warnings_on)

        true ->
          all
      end
    end
  end

  # A special case for mix, we need to figure out how to clean it up
  defp replace_fun(:mix_task, :elixir, :normal, options) do
    extension = options[:extension] || ".html"
    lib_dirs = options[:lib_dirs] || default_lib_dirs(:elixir)
    module_id = options[:module_id] || nil
    modules_refs = options[:modules_refs] || []

    fn all, text, task_name ->
      match = task_module(task_name)

      cond do
        match == module_id ->
          "[#{text}](#content)"

        match in modules_refs ->
          "[#{text}](#{match}#{extension})"

        doc = module_docs(:elixir, match, lib_dirs) ->
          "[#{text}](#{doc}#{match}#{extension})"

        true ->
          all
      end
    end
  end

  defp maybe_warn(text, match, module_id, id, skip_warnings_on) do
    if module_id not in skip_warnings_on and id not in skip_warnings_on do
      IO.warn(
        "documentation references #{match} but it doesn't exist " <>
          "or it's listed as @doc false (parsing #{id} docs)",
        []
      )
    end

    text
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

  defp split_arities(arities), do: String.split(arities, ",")
  defp arities_sorted?(arities), do: arities == Enum.sort(arities)

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
  defp re_source(name, language) do
    Regex.source(re(name, language))
  end

  defp re(:m, :elixir) do
    ~r{
      (
        (?:Elixir.)?   # optional Elixir prefix (not part of the module itself)
        ((?:[A-Z][_a-zA-Z0-9]*(?:\.[A-Z][_a-zA-Z0-9]*)*))
      )
    }x
  end

  defp re(:m, :erlang) do
    ~r{
      (
        :                          # prefix
        ([a-z][_a-zA-Z0-9]*)       # module_name
      )
    }x
  end

  defp re(:f, :elixir) do
    ~r{
      (
        (?:[a-z_][_a-zA-Z0-9]*[\?\!]?)     # regular function_name
        |                                  # OR
        [\{\}=&\\|\\.<>~*^@\\+\\%\\!-\/]+  # special_form
      )
    }x
  end

  defp re(:f, :erlang) do
    ~r{
      ([a-z][_a-zA-Z0-9]*)
    }x
  end

  defp re(:fa, language) when language in [:elixir, :erlang] do
    ~r{
      #{re_source(:f, language)}   # function_name
      /(\d+(?:,\d+)*)              # /arity
    }x
  end

  defp re_kind_language(:module, :elixir) do
    ~r{
      #{re_source(:m, :elixir)}
    }x
  end

  defp re_kind_language(:module, :erlang) do
    ~r{
      #{re_source(:m, :erlang)}
    }x
  end

  defp re_kind_language(:function, :elixir) do
    ~r{
      ([ct]:)?
      (?:#{re_source(:m, :elixir)}(\.))?
      #{re_source(:fa, :elixir)}
    }x
  end

  defp re_kind_language(:function, :erlang) do
    ~r{
      #{re_source(:m, :erlang)}            # module_name
      (\.)                                 # "."
      #{re_source(:fa, :erlang)}           # function_name/arity
    }x
  end

  defp re_kind_language(:mix_task, :elixir) do
    ~r{
      mix\ (?:help\ )?([a-z][a-z0-9\._]*)
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
          (?:#{source})                        # CAPTURE N
          \s*`)                                # trailing backtick
          (?!`)
          # (?!\)\])                           # it shouldn't be followed by ")]"
        }x

      :custom ->
        ~r{
          \[(.*?)\]            # CAPTURE 1
          \(`(?:#{source})`\)  # CAPTURE N
        }x
    end
  end

  defp basic_types_page_for(elixir_docs, extension) do
    "#{elixir_docs}typespecs#{extension}#basic-types"
  end

  defp built_in_types_page_for(elixir_docs, extension) do
    "#{elixir_docs}typespecs#{extension}#built-in-types"
  end
end
