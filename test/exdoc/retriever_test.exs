defmodule ExDoc.RetrieverTest do
  use ExUnit.Case, async: true

  alias ExDoc.Retriever

  defp config(url_pattern // "http://example.com/%{path}#L%{line}") do
    ExDoc.Config[source_url_pattern: url_pattern, source_root: File.cwd!]
  end

  defp docs_from_files(names, url_pattern // "http://example.com/%{path}#L%{line}") do
    files  = Enum.map names, fn(n) -> "test/tmp/Elixir.#{n}.beam" end
    Retriever.docs_from_files(files, config(url_pattern))
  end

  ## MODULES

  test "docs_from_files returns the module id" do
    [node] = docs_from_files ["CompiledWithDocs"]
    assert node.id == "CompiledWithDocs"
  end

  test "docs_from_files returns the module" do
    [node] = docs_from_files ["CompiledWithDocs"]
    assert node.module == CompiledWithDocs
  end

  test "docs_from_files returns the nested module" do
    [node] = docs_from_files ["UndefParent.Nested"]
    assert node.module == UndefParent.Nested
  end

  test "docs_from_files returns the relative module name" do
    [node] = docs_from_files ["UndefParent.Nested"]
    assert node.relative == "UndefParent.Nested"
  end

  test "docs_from_files returns the moduledoc info" do
    [node] = docs_from_files ["CompiledWithDocs"]
    assert node.moduledoc == "moduledoc\n\n\#\# Example\n    CompiledWithDocs.example\n"
  end

  test "docs_from_files returns nil if there's no moduledoc info" do
    [node] = docs_from_files ["CompiledWithoutDocs"]
    assert node.moduledoc == nil
  end

  test "docs_from_files returns the doc info for each module function" do
    [node] = docs_from_files ["CompiledWithDocs"]
    [ example, example_1, example_without_docs ] = node.docs

    assert example.id   == "example/2"
    assert example.doc  == "Some example"
    assert example.type == :def
    assert [{ :foo, _, _ }, { ://, _, _ }] = example.signature

    assert example_1.id   == "example_1/0"
    assert example_1.type == :defmacro

    assert example_without_docs.source == "http://example.com/test/fixtures/compiled_with_docs.ex\#L15"
    assert example_without_docs.doc == nil
  end

  test "docs_from_files returns an empty list if there's no docs info" do
    [node] = docs_from_files ["CompiledWithoutDocs"]
    assert node.docs == []
  end
  
  test "docs_from_files returns the specs for each non-private function" do
    [node] = docs_from_files ["TypesAndSpecs"]
    [add] = node.docs

    assert add.id     == "add/2"
    assert add.doc    == nil 
    assert add.type   == :def
    assert add.specs  == 
      [ ExDoc.SpecWithRefs[spec: "add(integer(), opaque()) :: integer()",
                           locals: [:opaque],
                           remotes: []] ]
  end
  
  test "docs_from_files returns the spec info for each non-private module type" do
    [node] = docs_from_files ["TypesAndSpecs"]
    [ opaque, public, ref ] = node.typespecs
    
    assert opaque.name         == "opaque"
    assert opaque.id           == "t:opaque"
    assert opaque.type         == :opaque
    assert opaque.spec.spec    == "opaque()"
    assert opaque.spec.locals  == [] 
    assert opaque.spec.remotes == []

    assert public.name         == "public"
    assert public.id           == "t:public"
    assert public.type         == :type
    assert public.spec.spec    ==
      "public(t) :: {t, String.t(), TypesAndSpecs.Sub.t(), opaque(), :ok | :error}"
    assert public.spec.locals  == [:opaque]
    assert public.spec.remotes == [{{String, :t}, :elixir}, {{TypesAndSpecs.Sub, :t}, :current}]
    
    assert ref.name            == "ref"
    assert ref.id              == "t:ref"
    assert ref.type            == :type
    assert ref.spec.spec       == "ref() :: {:binary.part(), public(any())}"
    assert ref.spec.locals     == [:public]
    assert ref.spec.remotes    == []
  end

  test "docs_from_files returns the source" do
    [node] = docs_from_files ["CompiledWithDocs"], "http://foo.com/bar/%{path}#L%{line}"
    assert node.source == "http://foo.com/bar/test/fixtures/compiled_with_docs.ex\#L1"
  end


  ## RECORDS

  test "docs_from_files properly tags records" do
    [node] = docs_from_files ["CompiledRecord"]
    assert node.type == :record
  end

  test "ignore records internal functions" do
    [node] = docs_from_files ["CompiledRecord"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == []
  end

  ## EXCEPTIONS

  test "docs_from_files properly tags exceptions" do
    [node] = docs_from_files ["RandomError"]
    assert node.type == :exception
  end

  test "ignore exceptions internal functions" do
    [node] = docs_from_files ["RandomError"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == []
  end

  ## BEHAVIOURS

  test "docs_from_files properly tags behaviours" do
    [node] = docs_from_files ["CustomBehaviour"]
    assert node.type == :behaviour
  end

  test "ignore behaviours internal functions" do
    [node] = docs_from_files ["CustomBehaviour"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["hello/1"]
    assert hd(node.docs).type == :defcallback
    assert hd(node.docs).signature == [{ :integer, [line: 7], [] }]
  end

  ## PROTOCOLS

  test "docs_from_files properly tag protocols" do
    [node] = docs_from_files ["CustomProtocol"]
    assert node.type == :protocol
  end

  test "ignore protocols internal functions" do
    [node] = docs_from_files ["CustomProtocol"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1", "plus_two/1"]
  end

  ## IMPLEMENTATIONS

  test "docs_from_files properly tag implementations" do
    [node]  = docs_from_files ["CustomProtocol.Number"]
    assert node.type == :impl
  end

  test "ignore impl internal functions" do
    [node]  = docs_from_files ["CustomProtocol.Number"]
    functions = Enum.map node.docs, fn(doc) -> doc.id end
    assert functions == ["plus_one/1", "plus_two/1"]
  end

  ## FILTERS

  test "filters modules" do
    assert [] == Retriever.filter_modules([], :modules)

    module_nil = ExDoc.ModuleNode[type: nil]
    behavior = ExDoc.ModuleNode[type: :behaviour]
    record = ExDoc.ModuleNode[type: :record]
    exception = ExDoc.ModuleNode[type: :exception]
    protocol = ExDoc.ModuleNode[type: :protocol]
    impl = ExDoc.ModuleNode[type: :impl]

    result = Retriever.filter_modules([module_nil, behavior, record, exception, protocol, impl], :modules)
    assert 2 == Enum.count result
    assert HashSet.new([module_nil, behavior]) == HashSet.new(result)
  end

  test "filters records" do
    assert [] == Retriever.filter_modules([], :records)

    module_nil = ExDoc.ModuleNode[type: nil]
    behavior = ExDoc.ModuleNode[type: :behaviour]
    record = ExDoc.ModuleNode[type: :record]
    exception = ExDoc.ModuleNode[type: :exception]
    protocol = ExDoc.ModuleNode[type: :protocol]
    impl = ExDoc.ModuleNode[type: :impl]

    result = Retriever.filter_modules([module_nil, behavior, record, exception, protocol, impl], :records)
    assert 2 == Enum.count result
    assert HashSet.new([record, exception]) == HashSet.new(result)
  end

  test "filters protocols" do
    assert [] == Retriever.filter_modules([], :protocols)

    module_nil = ExDoc.ModuleNode[type: nil]
    behavior = ExDoc.ModuleNode[type: :behaviour]
    record = ExDoc.ModuleNode[type: :record]
    exception = ExDoc.ModuleNode[type: :exception]
    protocol = ExDoc.ModuleNode[type: :protocol]
    impl = ExDoc.ModuleNode[type: :impl]

    result = Retriever.filter_modules([module_nil, behavior, record, exception, protocol, impl], :protocols)
    assert 2 == Enum.count result
    assert HashSet.new([protocol, impl]) == HashSet.new(result)
  end

  ## NEST MODULES

  test "nest_modules returns an empty list if there's no children" do
    nodes = docs_from_files ["CompiledWithDocs"]
    [node] = Retriever.nest_modules(nodes, config)
    assert node.children == []
  end

  test "nest_modules returns a list with children" do
    nodes = docs_from_files ["CompiledWithDocs", "CompiledWithDocs.Nested"]
    [node] = Retriever.nest_modules(nodes, config)
    [child] = node.children
    assert child.module   == CompiledWithDocs.Nested
    assert child.relative == "Nested"
  end

end
