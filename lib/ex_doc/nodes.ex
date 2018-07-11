defmodule ExDoc.ModuleNode do
  @moduledoc """
  Structure that represents a module.
  """

  defstruct id: nil,
            title: nil,
            module: nil,
            group: nil,
            deprecated: nil,
            doc: nil,
            doc_line: nil,
            docs: [],
            typespecs: [],
            source_path: nil,
            source_url: nil,
            type: nil

  @type t :: %__MODULE__{
          id: nil | String.t(),
          title: nil | String.t(),
          module: nil | String.t(),
          group: nil | String.t(),
          deprecated: nil | String.t(),
          docs: list(),
          doc: nil | String.t(),
          doc_line: non_neg_integer(),
          typespecs: list(),
          source_path: nil | String.t(),
          source_url: nil | String.t(),
          type: nil | :module | :exception | :protocol | :impl | :behaviour | :task
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
            doc: [],
            type: nil,
            signature: nil,
            specs: [],
            annotations: [],
            doc_line: nil,
            source_path: nil,
            source_url: nil

  @type t :: %__MODULE__{
          id: nil | String.t(),
          name: nil | String.t(),
          arity: non_neg_integer,
          defaults: non_neg_integer,
          doc: String.t(),
          doc_line: non_neg_integer,
          source_path: nil | String.t(),
          source_url: nil | String.t(),
          type: nil | String.t(),
          signature: nil | String.t(),
          specs: list(),
          annotations: list(),
          deprecated: nil | String.t()
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
            doc_line: nil,
            source_path: nil,
            source_url: nil,
            spec: nil,
            signature: nil,
            annotations: []

  @type t :: %__MODULE__{
          id: nil | String.t(),
          name: nil | String.t(),
          arity: non_neg_integer,
          type: nil | String.t(),
          spec: nil | String.t(),
          deprecated: nil | String.t(),
          doc: nil | String.t(),
          doc_line: non_neg_integer,
          signature: nil | String.t(),
          source_url: nil | String.t(),
          source_path: nil | String.t(),
          annotations: list()
        }
end
