defmodule ExDoc.EPUB.EntitiesTest do
  use ExUnit.Case, async: true

  alias ExDoc.EPUB.Entities

  test "converts HTML entities to numeric references" do
    assert Entities.to_numeric("&copy;&nbsp;&NotEqualTilde;") ==
             "&#x000A9;&#x000A0;&#x02242;&#x00338;"
  end

  test "preserves XML entities and numeric references" do
    assert Entities.to_numeric("&amp;&apos;&gt;&lt;&quot;&#169;&#xA9;") ==
             "&amp;&apos;&gt;&lt;&quot;&#169;&#xA9;"
  end

  test "escapes unknown entities and leaves bare ampersands unchanged" do
    assert Entities.to_numeric("one&two &unknown;") == "one&two &amp;unknown;"
  end

  test "only matches entity names up to the longest known name" do
    name = String.duplicate("a", 32)
    assert Entities.to_numeric("&#{name};") == "&#{name};"
  end
end
