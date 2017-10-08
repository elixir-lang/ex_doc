defmodule ExDoc.MarkdownProcessor do
  @moduledoc """
  ExDoc is compatible with markdown processors that implement this behaviour.

  If you want to write a custom markdown processor that plays well with ExDoc,
  you must implement this behaviour in your module.
  If you're not interested in writing a custom markdown processor for use with ExDoc,
  you can safely ignore this module.
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

  ## EPUB Documentation Gotchas

  Generating HTML documentation is simple, and it works exacly as you would expect for a webpage.
  The EPUB file format, on the other hand, may cause some surprise.

  Apparently, an EPUB file expects all assets to have a unique name *when discarding the file extension*.

  This creates problems if you include, for example, the files `custom.js` and `custom.css`.
  Because the filename without the extension is equal (`custom`), you will get an unreadable EPUB.
  It's possible to go around this limitation by simply giving the files unique names:

  - `custom.js` becomes `custom-js.js` *and*
  - `custom.css` becomes `custom-css.css`

  ## Example
      def callback assets(_) do
        [{"dist/custom-css.css", custom_css_content()},
         {"dist/custom-js.js", custom_js_content()}]
      end
  """
  @callback assets(atom) :: [{String.t, String.t}]

  @doc """
  Literal content to be written to the file just before the closing head tag.

  This callback takes the documentation format (`:html` or `epub`) as an argument
  and returns a literal string.
  It is useful when the markdown processor needs to a include extra CSS.

  ## Example
      def callback before_closing_head_tag(_) do
        # Include the CSS specified in the assets/1 callback
        ~S(<link rel="stylesheet" href="dist/custom-css.css"/>)
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
      def callback before_closing_body_tag(_) do
        # Include the Javascript specified in the assets/1 callback
        ~S(<script src="dist/custom-js.js"></script>)
      end
  """
  @callback before_closing_body_tag(atom) :: String.t

end
