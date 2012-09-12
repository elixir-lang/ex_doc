defrecord CompiledRecord, [foo: nil, bar: "sample"] do
  @moduledoc nil
end
defexception RandomError, message: "this is random!"