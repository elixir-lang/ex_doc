defmodule ExDoc.Language do
  @moduledoc false

  @type spec_ast() :: term()

  @typedoc """
  The map has the following keys:

    * `:module` - the module

    * `:docs` - the docs chunk

    * `:language` - the language callback

    * `:id` - module page name

    * `:title` - module display title

    * `:type` - module type

    * `:source_line` - the line where the module code is located, defmodule in Elixir, or -module in Erlang

    * `:source_file` - the source file the module code is located, defmodule in Elixir, or -module in Erlang

    * `:source_basedir` - the absolute directory where the Elixir/Erlang compiler was run.
      See `ExDoc.Language.Source.get_basedir/2` for more details.

    * `:callback_types` - a list of types that are considered callbacks

    * `:nesting_info` - a `{nested_title, nested_context}` tuple or `nil`.
      For example, `"A.B.C"` becomes `{"C", "A.B."}`.

    * `:private` - a map with language-specific data
  """
  @type module_data() :: %{
          module: module(),
          docs: tuple(),
          language: module(),
          id: String.t(),
          title: String.t(),
          type: atom() | nil,
          source_basedir: String.t(),
          source_file: String.t() | nil,
          source_line: non_neg_integer(),
          callback_types: [atom()],
          nesting_info: {String.t(), String.t()} | nil,
          private: map()
        }

  @doc """
  Returns a map with module information.
  """
  @callback module_data(module(), tuple(), ExDoc.Config.t()) :: module_data() | :skip

  @doc """
  Returns a map with function information or an atom `:skip`.

  The map has the following keys:

    * `:source_line` - the line where the code is located, def/defp in Elixir, foo(...) in Erlang

    * `:source_file` - the source file where the code in located

    * `:specs` - a list of specs that will be later formatted by `c:typespec/2`

    * `:doc_fallback` - if set, a 0-arity function that returns DocAST which
       will be used as fallback to empty docs on the function node

    * `:extra_annotations` - additional annotations

  """
  @callback function_data(entry :: tuple(), module_data()) ::
              %{
                source_line: non_neg_integer() | nil,
                source_file: String.t() | nil,
                specs: [spec_ast()],
                # TODO: change to following on Elixir 1.15. It trips mix formatter between 1.14 and 1.15
                # doc_fallback: (-> ExDoc.DocAST.t()) | nil,
                doc_fallback: (... -> ExDoc.DocAST.t()) | nil,
                extra_annotations: [String.t()]
              }
              | :skip

  @doc """
  Returns a map with callback information.

  The map has the following keys:

    * `:source_line` - the line where the code is located

    * `:source_file` - the source file where the code in located

    * `:signature` - the signature

    * `:specs` - a list of specs that will be later formatted by `c:typespec/2`

    * `:extra_annotations` - additional annotations

  """
  @callback callback_data(entry :: tuple(), module_data()) ::
              %{
                source_line: non_neg_integer() | nil,
                source_file: String.t() | nil,
                signature: [binary()],
                specs: [spec_ast()],
                extra_annotations: [String.t()]
              }

  @doc """
  Returns a map with type information.

  The map has the following keys:

    * `:type` - `:type` or `:opaque`

    * `:source_line` - the line where the code is located

    * `:source_file` - the source file where the code in located

    * `:signature` - the signature

    * `:spec` - a spec that will be later formatted by `c:typespec/2`
  """
  @callback type_data(entry :: tuple(), spec :: term()) ::
              %{
                type: :type | :opaque,
                source_line: non_neg_integer(),
                source_file: String.t() | nil,
                signature: [binary()],
                spec: spec_ast(),
                extra_annotations: [String.t()]
              }

  @doc """
  Autolinks docs.
  """
  @callback autolink_doc(doc :: ExDoc.DocAST.t(), opts :: keyword()) :: ExDoc.DocAST.t()

  @doc """
  Autolinks typespecs.
  """
  @callback autolink_spec(spec :: term(), opts :: keyword()) :: iodata()

  @doc """
  Returns information for syntax highlighting.
  """
  @callback highlight_info() :: %{
              language_name: String.t(),
              lexer: module(),
              opts: keyword()
            }

  @doc """
  Return an attribute in the canonical representation.
  """
  @callback format_spec_attribute(%ExDoc.FunctionNode{} | %ExDoc.TypeNode{}) :: String.t()

  @doc """
  Parse a module.function string and return it.
  """
  @callback parse_module_function(String.t()) ::
              {:local, function :: atom()}
              | {:remote, module :: module(), function :: atom()}
              | :error

  @doc """
  Parse a module string and return it.
  """
  @callback parse_module(String.t(), mode :: :regular_link | :custom_link) ::
              {:module, atom()} | :error

  @doc """
  Return a URL to autoimported function if atom+arity are autoimported
  """
  @callback try_autoimported_function(
              name :: atom(),
              arity :: non_neg_integer(),
              mode :: :regular_link | :custom_link,
              opts :: keyword(),
              original_text :: String.t()
            ) ::
              nil | String.t()

  @doc """
  Return a URL to built-in type if atom+arity are built-in
  """
  @callback try_builtin_type(
              name :: atom(),
              arity :: non_neg_integer(),
              mode :: :regular_link | :custom_link,
              opts :: keyword(),
              original_text :: String.t()
            ) ::
              nil | String.t()

  def get(:elixir, _module), do: {:ok, ExDoc.Language.Elixir}
  def get(:erlang, _module), do: {:ok, ExDoc.Language.Erlang}

  def get(language, module) when is_atom(language) and is_atom(module) do
    ExDoc.Utils.warn(
      "skipping module #{module}, reason: unsupported language (#{language})",
      []
    )

    :error
  end
end
