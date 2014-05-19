defrecord CompiledRecord, [foo: nil, bar: "sample"] do
  @moduledoc "Hello"
end
defmodule RandomError do
  defexception [:message]
end
