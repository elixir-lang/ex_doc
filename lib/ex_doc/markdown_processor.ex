defmodule ExDoc.MarkdownProcessor do
  @moduledoc """
  ExDoc is compatible with markdown processors that implement this behaviour.
  """

  @doc """
  Converts markdown into HTML.
  """
  @callback to_html(String.t, Keyword.t) :: String.t
  
  @doc """
  Assets specific to the markdown implementation.
  
  This callback takes the documentation format (`:html` or `epub`) as an argument
  and must return a list of pairs of the form: `{basename, content}` where:
  
  - `basename` relative path that will be written inside the `doc/` directory.
  - `content` is a binary with the full contents of the file that will be written to `basename`.

  ## Example
      def callback before_closing_head_tag(:html) do
        [{"dist/custom.css", custom_css_content()},
         {"dist/custom.js", custom_js_content()}]
      end 
  """
  @callback assets(atom) :: [{String.t, String.t}]

  @doc """
  Literal content to be written to the file just before the closing head tag.

  This callback takes the documentation format (`:html` or `epub`) as an argument
  and returns a literal string.
  It is useful when the markdown processor needs to a include extra CSS.

  ## Example
      def callback before_closing_head_tag(:html) do
        # Include the Javascript specified in the assets/1 callback
        ~S(<link rel="stylesheet" href="dist/custom.css"/>)
      end 
  """
  @callback before_closing_head_tag(atom) :: String.t
  
  @doc """
  Literal content to be written to the file just before the closing body tag.

  This callback takes the documentation format (`:html` or `epub`) as an argument
  and returns a literal string.
  It is useful when the markdown processor needs to a include extra Javascript.
  The result of this function is written directly to the output file.
  
  ## Example
      def callback before_closing_body_tag(:html) do
        # Include the Javascript specified in the assets/1 callback
        ~S(<link rel="stylesheet" href="dist/custom.js"/>)
      end 
  """
  @callback before_closing_body_tag(atom) :: String.t

end
