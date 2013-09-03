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
    guide = generate_documents(output, config.guide_files, :guide)
    # Sort the misc files section on title (original filename for files).
    misc = generate_documents(output, config.misc_files, :misc)
           |> Enum.sort(&(elem(&1, 1) < elem(&2, 1))) 
    documents = [guide: guide, misc: misc]
    has_readme = config.readme && generate_readme(output)

    modules = Autolink.all(modules)

    Enum.each [:modules, :records, :protocols], fn(mod_type) ->
      generate_list(mod_type, modules, output, config, has_readme, documents)
    end
    generate_document_lists(output, config, has_readme, documents)
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

  defp generate_documents(output, files, type) do
    guide_files = Enum.flat_map(files, &Path.wildcard/1)
    case guide_files do
      [] -> []
      l  -> 
        lc f inlist l do
          dirparts = Path.split(Path.relative_to_cwd(Path.dirname(f)))
          basename_without_ext = Path.rootname(Path.basename(f))
          # Ugly stuff to try to avoid creating problematic URL's and filesystem
          # locations. Pretty unlikely that anyone will ever trigger this, but
          # you never know.
          parts = dirparts
                  |> Enum.filter(&(&1 != "/"))
                  |> Enum.map(fn s -> if s == "..", do: "__up__", else: s end)
                  |> Enum.map(&escape_url_part/1)
          all_parts = parts ++ [escape_url_part(basename_without_ext) <> ".html"]
          File.mkdir_p!(Path.join([output | parts]))
          filename = Path.join([output | all_parts])
          refpath = Enum.join(all_parts, "/")
          # Ok, very ugly, we need to process the template in order to get the
          # title (it's extracted from the HTML). We'll pass an empty title now
          # and then splice in the title in post processing.
          contents = File.read!(f)
          html = Templates.document_template("", contents)
          { html, title, sections } = post_process_document(html)
          File.write!(filename, html)
          case type do
            :guide -> { refpath, title, sections }
            # Misc files are identified by their filenames and don't have their
            # sections stored. This prevents the files getting a tree structure,
            # files are usually short enough to not warrant jumping to sections
            # through the side bar.
            :misc -> { refpath, Templates.h(Path.relative_to_cwd(f)), [] }
          end
        end
    end
  end

  defp escape_url_part(s) do
    String.to_char_list!(s)
    |> Enum.flat_map(
        fn c when c in ?a..?z or c in ?A..?Z or c in ?0..?9 or c in [?., ?-, ?_] -> [c]
           c -> [?%|integer_to_list(c, 16)]
        end)
    |> String.from_char_list!()
  end

  def post_process_document(html) do
    # A section id is simply the text in HTML with all entities removed
    # and spaces converted to underscores.
    secs = lc [h, t] inlist Regex.scan(%r{<h2>(.*?)</h2>}g, html) do
             { h, t, String.replace(t, %r{&(.*?);}, "") |> String.replace(" ", "_") |> String.downcase }
           end
    title = case Regex.run(%r{<h1>(.*?)</h1>}g, html) do
              # Some documents don't have their title as a h1 heading but as a
              # h2, so fall back to the first h2 if there's no h1.
              nil    -> case secs do
                          [{ _, t, _ }|_] -> t
                          [] -> "(unknown)"
                        end
              [_, t] -> t 
            end
    # Add id's so that direct links to them from the sidebar are possible. 
    html = Enum.reduce(secs, html, fn { h, inner, id }, acc ->
             String.replace(acc, h, "<h2 id=\"#{id}\">#{inner}</h2>")
           end)
    html = String.replace(html, "<title></title>", "<title>#{title}</title>")
    { html, title, (lc { _, sec_title, id } inlist secs, do: { sec_title, id }) }
  end
  
  def generate_document_lists(output, config, has_readme, documents) do
    contents = Templates.list_page(:guide, [], config, has_readme, documents)
    File.write("#{output}/guide_list.html", contents)
    contents = Templates.list_page(:misc, [], config, has_readme, documents)
    File.write("#{output}/files_list.html", contents)
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

  defp generate_list(scope, all, output, config, has_readme, documents) do
    nodes = filter_list(scope, all)
    Enum.each nodes, &generate_module_page(&1, output)
    content = Templates.list_page(scope, nodes, config, has_readme, documents)
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
