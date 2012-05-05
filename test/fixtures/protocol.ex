defprotocol CustomProtocol do
  def plus_one(foo)
end

defimpl CustomProtocol, for: Number do
  def plus_one(number), do: number + 1
end