defmodule ExDoc.Formatter.EPUB.Templates do
  @moduledoc """
  Handle all template interfaces for the EPUB formatter.
  """

  require EEx

  alias ExDoc.Formatter.HTML
  alias ExDoc.Formatter.HTML.Templates, as: H

  @doc """
  Generate content from the module template for a given `node`
  """
  def module_page(config, module_node) do
    summary_map = H.group_summary(module_node)
    module_template(config, module_node, summary_map)
  end

  @doc """
  Creates the [Package Document Definition](http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-package-def),
  this definition encapsulates the publication metadata and the resource
  information that constitute the EPUB publication. This definition also
  includes the default reading order.
  """
  EEx.function_from_file(:def, :content_template,
                         Path.expand("templates/content_template.eex", __DIR__),
                         [:config, :nodes, :uuid, :datetime, :static_files])

  @doc """
  Creates a chapter which contains all the details about an individual module,
  this chapter can include the following sections: *functions*, *macros*,
  *types*, *callbacks*.
  """
  EEx.function_from_file(:def, :module_template,
                         Path.expand("templates/module_template.eex", __DIR__),
                         [:config, :module, :summary_map])

  @doc """
  Creates the table of contents. This template follows the
  [EPUB Navigation Document Definition](http://www.idpf.org/epub/30/spec/epub30-contentdocs.html#sec-xhtml-nav).
  """
  EEx.function_from_file(:def, :nav_template,
                         Path.expand("templates/nav_template.eex", __DIR__),
                         [:config, :nodes])

  @doc """
  Creates a new chapter when the user provides additional files.
  """
  EEx.function_from_file(:def, :extra_template,
                         Path.expand("templates/extra_template.eex", __DIR__),
                         [:config, :title, :content])

  @doc """
  Creates the cover page for the EPUB document.
  """
  EEx.function_from_file(:def, :title_template,
                         Path.expand("templates/title_template.eex", __DIR__),
                         [:config])

  @doc """
  Creates an *Navigation Center eXtended* document (as defined in OPF 2.0.1),
  this is for compatibility purposes with EPUB 2 Reading Systems.  EPUB 3
  Reading Systems must ignore the NCX in favor of the
  [EPUB Navigation Document](http://www.idpf.org/epub/30/spec/epub30-contentdocs.html#sec-xhtml-nav).
  """
  EEx.function_from_file(:def, :toc_template,
                         Path.expand("templates/toc_template.eex", __DIR__),
                         [:config, :nodes, :uuid])

  EEx.function_from_file(:defp, :head_template,
                         Path.expand("templates/head_template.eex", __DIR__),
                         [:config, :page])

  "templates/media-types.txt"
  |> Path.expand(__DIR__)
  |> File.read!()
  |> String.split("\n", trim: true)
  |> Enum.each(fn(line) ->
    [extension, media] = String.split(line, ",")

    defp media_type("." <> unquote(extension)) do
      unquote(media)
    end
  end)

  defp media_type(arg), do: raise "asset with extension #{inspect arg} is not supported by EPUB format"
end
