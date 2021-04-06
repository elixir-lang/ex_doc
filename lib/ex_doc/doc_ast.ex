defmodule ExDoc.DocAST do
  @moduledoc false

  alias ExDoc.Markdown
  alias ExDoc.Formatter.HTML.Templates

  @doc """
  Parse given `doc` according to `doc_format`.
  """
  def parse!(doc, doc_format, options \\ [])

  def parse!(markdown, "text/markdown", opts) do
    parse_markdown(markdown, opts)
  end

  def parse!(ast, "application/erlang+html", _options) do
    parse_erl_ast(ast)
  end

  def parse!(_ast, other, _opts) do
    raise "content type #{inspect(other)} is not supported"
  end

  # https://www.w3.org/TR/2011/WD-html-markup-20110113/syntax.html#void-element
  @void_elements ~W(area base br col command embed hr img input keygen link 
    meta param source track wbr)a

  @doc """
  Transform AST into string.
  """
  def to_string(ast, fun \\ fn _ast, string -> string end)

  def to_string(binary, _fun) when is_binary(binary) do
    Templates.h(binary)
  end

  def to_string(list, fun) when is_list(list) do
    result = Enum.map_join(list, "", &to_string(&1, fun))
    fun.(list, result)
  end

  def to_string({tag, attrs, _inner, _meta} = ast, fun) when tag in @void_elements do
    result = "<#{tag}#{ast_attributes_to_string(attrs)}/>"
    fun.(ast, result)
  end

  def to_string({tag, attrs, inner, %{verbatim: true}} = ast, fun) do
    inner = Enum.join(inner, "")
    result = "<#{tag}#{ast_attributes_to_string(attrs)}>" <> inner <> "</#{tag}>"
    fun.(ast, result)
  end

  def to_string({tag, attrs, inner, _meta} = ast, fun) do
    result = "<#{tag}#{ast_attributes_to_string(attrs)}>" <> to_string(inner, fun) <> "</#{tag}>"
    fun.(ast, result)
  end

  defp ast_attributes_to_string(attrs) do
    Enum.map(attrs, fn {key, val} -> " #{key}=\"#{val}\"" end)
  end

  ## parse markdown

  defp parse_markdown(markdown, opts) do
    Markdown.to_ast(markdown, opts)
  end

  ## parse erlang+html

  defp parse_erl_ast(binary) when is_binary(binary) do
    binary
  end

  defp parse_erl_ast(list) when is_list(list) do
    Enum.map(list, &parse_erl_ast/1)
  end

  defp parse_erl_ast({tag, attrs, content}) when is_atom(tag) do
    {tag, attrs, parse_erl_ast(content), %{}}
  end
end
