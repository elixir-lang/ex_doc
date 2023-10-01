defmodule ExDoc.ModuleNode do
  @moduledoc """
  Structure that represents a module.
  """

  defstruct id: nil,
            title: nil,
            nested_context: nil,
            nested_title: nil,
            module: nil,
            group: nil,
            deprecated: nil,
            doc_format: nil,
            doc: nil,
            source_doc: nil,
            rendered_doc: nil,
            doc_line: nil,
            docs_groups: [],
            docs: [],
            typespecs: [],
            source_path: nil,
            source_url: nil,
            type: nil,
            language: nil,
            annotations: [],
            metadata: nil

  @typep annotation :: atom()

  @type t :: %__MODULE__{
          id: String.t(),
          title: String.t(),
          nested_context: String.t() | nil,
          nested_title: String.t() | nil,
          module: module(),
          group: atom() | nil,
          deprecated: String.t() | nil,
          doc_format: String.t() | nil,
          doc: ExDoc.DocAST.t() | nil,
          source_doc: term(),
          rendered_doc: String.t() | nil,
          doc_line: non_neg_integer(),
          docs_groups: [atom()],
          docs: [ExDoc.FunctionNode.t()],
          typespecs: [ExDoc.TypeNode.t()],
          source_path: String.t(),
          source_url: String.t() | nil,
          type: atom(),
          language: module(),
          annotations: [annotation()],
          metadata: map()
        }
end

defmodule ExDoc.FunctionNode do
  @moduledoc """
  Structure that represents an individual function.
  """

  defstruct id: nil,
            name: nil,
            arity: 0,
            defaults: [],
            deprecated: nil,
            doc: nil,
            source_doc: nil,
            rendered_doc: nil,
            type: nil,
            signature: nil,
            specs: [],
            annotations: [],
            group: nil,
            doc_line: nil,
            source_path: nil,
            source_url: nil

  @typep annotation :: String.t()
  @typep function_default :: {name :: atom(), arity :: non_neg_integer()}

  @type t :: %__MODULE__{
          id: String.t(),
          name: atom(),
          arity: non_neg_integer(),
          defaults: [function_default()],
          deprecated: String.t() | nil,
          doc: ExDoc.DocAST.t() | nil,
          source_doc: String.t() | nil,
          rendered_doc: String.t() | nil,
          type: atom(),
          signature: String.t(),
          specs: [ExDoc.Language.spec_ast()],
          annotations: [annotation()],
          group: atom() | nil,
          doc_line: non_neg_integer(),
          source_path: String.t(),
          source_url: String.t() | nil
        }
end

defmodule ExDoc.TypeNode do
  @moduledoc """
  Structure that represents an individual type.
  """

  defstruct id: nil,
            name: nil,
            arity: 0,
            type: nil,
            deprecated: nil,
            doc: nil,
            source_doc: nil,
            rendered_doc: nil,
            doc_line: nil,
            source_path: nil,
            source_url: nil,
            spec: nil,
            signature: nil,
            annotations: []

  @typep annotation :: String.t()

  @type t :: %__MODULE__{
          id: String.t(),
          name: atom(),
          arity: non_neg_integer(),
          type: atom(),
          deprecated: nil,
          doc: ExDoc.DocAST.t() | nil,
          source_doc: String.t() | nil,
          rendered_doc: String.t() | nil,
          doc_line: non_neg_integer(),
          source_path: String.t(),
          source_url: String.t() | nil,
          spec: ExDoc.Language.spec_ast(),
          signature: String.t(),
          annotations: [annotation()]
        }
end
