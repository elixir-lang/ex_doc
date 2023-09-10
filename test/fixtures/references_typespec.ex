defmodule ReferencesTypespec do
  @moduledoc """
  references public typespecs
  """

  @spec a() :: TypesAndSpecs.public(integer())
  def a do
    10
  end
end
