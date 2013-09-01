defmodule ExDoc.HTMLFormatter do
  @moduledoc """
  Provide HTML-formatted documentation
  """

  alias ExDoc.HTMLFormatter.Templates
  alias ExDoc.HTMLFormatter.Autolink

  @doc """
  Generate HTML documentation for the given modules
  """
  def run(modules, config)  do
    output = Path.expand(config.output)
    File.mkdir_p output

    generate_index(output, config)
    generate_assets(output, config)
    guide = generate_guide(output, config)
    has_readme = config.readme && generate_readme(output)

    modules = Autolink.all(modules)

    Enum.each [:modules, :records, :protocols], fn(mod_type) ->
      generate_list(mod_type, modules, output, config, has_readme, guide)
    end
    generate_guide_list(output, config, has_readme, guide)
  end

  defp generate_index(output, config) do
    content = Templates.index_template(config)
    File.write("#{output}/index.html", content)
  end

  defp assets do
    [ { templates_path("css/*.css"), "css" },
      { templates_path("js/*.js"), "js" } ]
  end

  defp generate_assets(output, _config) do
    Enum.each assets, fn({ pattern, dir }) ->
      output = "#{output}/#{dir}"
      File.mkdir output

      Enum.map Path.wildcard(pattern), fn(file) ->
        base = Path.basename(file)
        File.copy file, "#{output}/#{base}"
      end
    end
  end

  defp generate_guide(output, config) do
    guide_files = expand_guide_files(config.guide_files)
    case guide_files do
      [] -> []
      l  -> 
        guide_dir = Path.join(output, "guide")
        File.mkdir_p!(guide_dir)
        lc f inlist l do
          contents = File.read!(f)
          filename = Path.join(guide_dir, "#{Path.basename(f, ".md")}.html")
          # Ok, very ugly, we need to process the template in order to get the
          # title (it's extracted from the HTML). We'll pass an empty title now
          # and then splice in the title in post processing.
          html = Templates.document_template("", contents)
          { html, title, sections } = post_process_document(html)
          File.write!(filename, html) 
          { filename, title, sections }
        end
    end
  end

  defp expand_guide_files(l) do
    List.flatten(lc f inlist l do
      if File.dir?(f) do 
        File.ls!(f) |> Enum.sort |> Enum.map(Path.join(f, &1))
          |> Enum.filter(&(Path.extname(&1) == ".md" and File.regular?(&1)))
      else
        f
      end
    end)
  end

  def post_process_document(html) do
    title = case Regex.run(%r{<h1>(.*?)</h1>}g, html) do
              nil    -> "(unknown)"
              [_, t] -> t 
            end
    # A section id is simply the text in HTML with all entities removed
    # and spaces converted to underscores.
    secs = lc [h, t] inlist Regex.scan(%r{<h2>(.*?)</h2>}g, html) do
             { h, t, String.replace(t, %r{&(.*?);}, "") |> String.replace(" ", "_") |> String.downcase }
           end
    # Add id's so that direct links to them from the sidebar are possible. 
    html = Enum.reduce(secs, html, fn { h, inner, id }, acc ->
             String.replace(acc, h, "<h2 id=\"#{id}\">#{inner}</h2>")
           end)
    html = String.replace(html, "<title></title>", "<title>#{title}</title>")
    { html, title, (lc { _, sec_title, id } inlist secs, do: { sec_title, id }) }
  end
  
  def generate_guide_list(output, config, has_readme, guide) do
    contents = Templates.list_page(:guide, [], config, has_readme, guide)
    File.write("#{output}/guide_list.html", contents)
  end

  defp generate_readme(output) do
    File.rm("#{output}/README.html")
    write_readme(output, File.read("README.md"))
  end

  defp write_readme(output, {:ok, content}) do
    readme_html = Templates.document_template("README", content)
    File.write("#{output}/README.html", readme_html)
    true
  end

  defp write_readme(_, _) do
    false
  end

  defp filter_list(:records, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [:record, :exception], &1)
  end

  defp filter_list(:modules, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [nil, :behaviour], &1)
  end

  defp filter_list(:protocols, nodes) do
    Enum.filter nodes, &match?(ExDoc.ModuleNode[type: x] when x in [:protocol], &1)
  end

  defp generate_list(scope, all, output, config, has_readme, guide) do
    nodes = filter_list(scope, all)
    Enum.each nodes, &generate_module_page(&1, output)
    content = Templates.list_page(scope, nodes, config, has_readme, guide)
    File.write("#{output}/#{scope}_list.html", content)
  end

  defp generate_module_page(node, output) do
    content = Templates.module_page(node)
    File.write("#{output}/#{node.id}.html", content)
  end

  defp templates_path(other) do
    Path.expand("html_formatter/templates/#{other}", __DIR__)
  end
end
