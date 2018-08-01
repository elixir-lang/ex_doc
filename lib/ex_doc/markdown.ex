defmodule ExDoc.Markdown do
  @moduledoc """
  Adapter behaviour and conveniences for converting Markdown to HTML.

  ExDoc is compatible with any markdown processor that implements the
  functions defined in this module. The markdown processor can be changed
  via the `:markdown_processor` option in your `mix.exs` or via the
  `:markdown_processor` configuration in the `:ex_doc` configuration.

  ExDoc supports the following Markdown parsers out of the box:

    * [Earmark](http://github.com/pragdave/earmark)
    * [Cmark](https://github.com/asaaki/cmark.ex)

  ExDoc uses Earmark by default.
  """

  @doc """
  Converts markdown into HTML.
  """
  @callback to_html(String.t(), Keyword.t()) :: String.t()

  @doc """
  Assets specific to the markdown implementation.

  This callback takes the documentation format (`:html` or `:epub`) as an argument
  and must return a list of pairs of the form `{basename, content}` where:

    * `basename` - relative path that will be written inside the `doc/` directory.
    * `content` - is a binary with the full contents of the file that will be written to `basename`.

  ## EPUB Documentation Gotchas

  Generating HTML documentation is simple, and it works exacly as you would expect
  for a webpage. The EPUB file format, on the other hand, may cause some surprise.

  Apparently, an EPUB file expects all assets to have a unique name *when
  discarding the file extension*.

  This creates problems if you include, for example, the files `custom.js`
  and `custom.css`. Because the filename without the extension is equal (`custom`),
  you will get an unreadable EPUB. It's possible to go around this limitation by simply
  giving the files unique names:

    * `custom.js` becomes `custom-js.js` *and*
    * `custom.css` becomes `custom-css.css`

  ## Example

      def callback assets(_) do
        [{"dist/custom-css.css", custom_css_content()},
         {"dist/custom-js.js", custom_js_content()}]
      end

  """
  @callback assets(atom) :: [{String.t(), String.t()}]

  @doc """
  Literal content to be written to the file just before the closing head tag.

  This callback takes the documentation format (`:html` or `:epub`) as an argument
  and returns a literal string. It is useful when the markdown processor needs to
  a include extra CSS.

  ## Example

      def callback before_closing_head_tag(_) do
        # Include the CSS specified in the assets/1 callback
        ~S(<link rel="stylesheet" href="dist/custom-css.css"/>)
      end

  """
  @callback before_closing_head_tag(atom) :: String.t()

  @doc """
  Literal content to be written to the file just before the closing body tag.

  This callback takes the documentation format (`:html` or `:epub`) as an argument
  and returns a literal string. It is useful when the markdown processor needs to
  a include extra JavaScript.

  ## Example

      def callback before_closing_body_tag(_) do
        # Include the Javascript specified in the assets/1 callback
        ~S(<script src="dist/custom-js.js"></script>)
      end

  """
  @callback before_closing_body_tag(atom) :: String.t()

  @doc """
  A function that accepts configuration options and configures the markdown processor.

  It is run once when `:ex_doc` is loaded, and the return value is discarded.
  Modules that implement this behaviour will probably store the options somewhere
  so that they can be accessed when needed.

  The format of the options as well as what the function does with them is
  completely up to the module that implements the behaviour.
  """
  @callback configure(any) :: :ok

  @markdown_processors [
    ExDoc.Markdown.Hoedown,
    ExDoc.Markdown.Earmark,
    ExDoc.Markdown.Cmark
  ]

  @markdown_processor_key :markdown_processor

  @doc """
  Converts the given markdown document to HTML.
  """
  def to_html(text, opts \\ []) when is_binary(text) do
    get_markdown_processor().to_html(text, opts)
  end

  @doc """
  Gets the current markdown processor set globally.
  """
  def get_markdown_processor do
    case Application.fetch_env(:ex_doc, @markdown_processor_key) do
      {:ok, processor} ->
        processor

      :error ->
        processor = find_markdown_processor() || raise_no_markdown_processor()
        put_markdown_processor(processor)
        processor
    end
  end

  @doc """
  Changes the markdown processor globally.
  """
  def put_markdown_processor(processor) do
    Application.put_env(:ex_doc, @markdown_processor_key, processor)
  end

  @doc false
  def configure_processor(options) do
    # This function configures the markdown processor with the given options.
    # It's called exactly once when ExDoc reads its own configuration options.
    # It's supposed to be called for its side-effects.
    get_markdown_processor().configure(options)
  end

  defp find_markdown_processor do
    Enum.find(@markdown_processors, fn module ->
      Code.ensure_loaded?(module) && module.available?
    end)
  end

  defp raise_no_markdown_processor do
    raise """
    Could not find a markdown processor to be used by ex_doc.
    You can either:

    * Add {:earmark, ">= 0.0.0"} to your mix.exs deps
      to use an Elixir-based markdown processor

    * Add {:markdown, github: "devinus/markdown"} to your mix.exs deps
      to use a C-based markdown processor

    * Add {:cmark, ">= 0.5"} to your mix.exs deps
      to use another C-based markdown processor
    """
  end
end
