defmodule ExDoc.TestHTMLHelpers do
  use ExUnit.Case

  def findElem(content, selector) do
    hd(Floki.find(content, selector))
  end

  def assertText(el, expected) do
    actual = Floki.text(el)
    assert actual === expected
  end

  def assertAttribute(el, attribute, expected) do
    actual = hd(Floki.attribute(el, attribute))
    assert actual === expected
  end

  def exists(content, selector) do
    assert length(Floki.find(content, selector)) > 0
  end
end
