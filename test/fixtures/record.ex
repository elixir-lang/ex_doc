defrecord CompiledRecord, [foo: nil, bar: "sample"] do
  @moduledoc "Hello"
end
defexception RandomError, message: "this is random!"