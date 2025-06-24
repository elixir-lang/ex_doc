# TODO: source_doc should only be a string once we remove application/html+erlang.
defmodule ExDoc.ModuleNode do
  @moduledoc false

  defstruct id: nil,
            title: nil,
            nested_context: nil,
            nested_title: nil,
            module: nil,
            group: nil,
            deprecated: nil,
            doc: nil,
            source_doc: nil,
            source_format: nil,
            moduledoc_line: nil,
            moduledoc_file: nil,
            source_path: nil,
            source_url: nil,
            docs_groups: [],
            typespecs: [],
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
          doc: ExDoc.DocAST.t() | nil,
          source_doc: term() | nil,
          source_format: String.t() | nil,
          moduledoc_line: non_neg_integer(),
          moduledoc_file: String.t(),
          source_path: String.t() | nil,
          source_url: String.t() | nil,
          docs_groups: [ExDoc.DocGroupNode.t()],
          typespecs: [ExDoc.DocNode.t()],
          type: atom(),
          language: module(),
          annotations: [annotation()],
          metadata: map()
        }
end

defmodule ExDoc.DocNode do
  @moduledoc false

  defstruct id: nil,
            name: nil,
            arity: 0,
            defaults: [],
            deprecated: nil,
            doc: nil,
            source_doc: nil,
            type: nil,
            signature: nil,
            specs: [],
            annotations: [],
            group: nil,
            doc_line: nil,
            doc_file: nil,
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
          source_doc: term() | nil,
          type: atom(),
          signature: String.t(),
          specs: [ExDoc.Language.spec_ast() | String.t()],
          annotations: [annotation()],
          group: String.t() | nil,
          doc_file: String.t(),
          doc_line: non_neg_integer(),
          source_url: String.t() | nil
        }
end

defmodule ExDoc.DocGroupNode do
  @moduledoc false
  defstruct title: nil, description: nil, doc: nil, docs: []

  @type t :: %__MODULE__{
          title: String.t() | atom(),
          description: String.t() | nil,
          doc: ExDoc.DocAST.t() | nil,
          docs: [ExDoc.DocNode.t()]
        }
end
