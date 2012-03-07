require 'redcarpet'

markdown = Redcarpet::Markdown.new(Redcarpet::Render::HTML,:autolink => true, :space_after_headers => true)

Dir['html/*.md'].each do |file|
  content = markdown.render(File.read(file))
  File.open(file + ".html", "w") { |f| f.write(content) }
end
