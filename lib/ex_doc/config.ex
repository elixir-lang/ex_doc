defmodule ExDoc.Config do
  @moduledoc false

  @default_formatter "html"
  @default_source_ref "master"

  def default_source_ref do
    @default_source_ref
  end

  def default_formatter do
    @default_formatter
  end

  def before_closing_head_tag(_), do: ""
  def before_closing_body_tag(_), do: ""

  defstruct assets: nil,
            before_closing_head_tag: &__MODULE__.before_closing_head_tag/1,
            before_closing_body_tag: &__MODULE__.before_closing_body_tag/1,
            canonical: nil,
            deps: [],
            extra_section: nil,
            extras: [],
            filter_prefix: nil,
            formatter: @default_formatter,
            groups_for_extras: [],
            groups_for_modules: [],
            homepage_url: nil,
            language: "en",
            logo: nil,
            main: nil,
            output: "./doc",
            project: nil,
            retriever: ExDoc.Retriever,
            source_beam: nil,
            source_ref: @default_source_ref,
            source_root: nil,
            source_url: nil,
            source_url_pattern: nil,
            title: nil,
            version: nil

  @type t :: %__MODULE__{
          assets: nil | String.t(),
          before_closing_head_tag: (atom() -> String.t()),
          before_closing_body_tag: (atom() -> String.t()),
          canonical: nil | String.t(),
          deps: [{ebin_path :: String.t(), doc_url :: String.t()}],
          extra_section: nil | String.t(),
          extras: list(),
          groups_for_extras: keyword(),
          filter_prefix: nil | String.t(),
          formatter: nil | String.t(),
          homepage_url: nil | String.t(),
          language: String.t(),
          logo: nil | Path.t(),
          main: nil | String.t(),
          groups_for_modules: keyword(),
          output: nil | Path.t(),
          project: nil | String.t(),
          retriever: :atom,
          source_beam: nil | String.t(),
          source_ref: nil | String.t(),
          source_root: nil | String.t(),
          source_url: nil | String.t(),
          source_url_pattern: nil | String.t(),
          title: nil | String.t(),
          version: nil | String.t()
        }
end
