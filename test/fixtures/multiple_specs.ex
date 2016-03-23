defmodule MultipleSpecs do

  @spec range?(%Range{}) :: true
  @spec range?(term) :: false
  def range?(term)
  def range?(%Range{}), do: true
  def range?(_), do: false

end
