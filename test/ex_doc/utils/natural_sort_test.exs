defmodule ExDoc.Utils.NaturalSort do
  use ExUnit.Case, async: true

  @input ~w|
    reraise/2
    reraise/3
    send/2
    sigil_C/2
    sigil_D/2
    sigil_c/2
    λ/1
    spawn/1
    spawn/3
    Λ/1
    abc/1
    a2/1
    a1/1
    a0/1
    a10/1
    a1_0/1
    a_0/1
    äpfeln/1
    Äpfeln/1
    умножить/1
    Вычесть/1
    вычесть/1
  |

  @sorted ~w|
    Äpfeln/1
    a0/1
    a1/1
    a1_0/1
    a2/1
    a10/1
    a_0/1
    abc/1
    äpfeln/1
    reraise/2
    reraise/3
    send/2
    sigil_C/2
    sigil_c/2
    sigil_D/2
    spawn/1
    spawn/3
    Λ/1
    λ/1
    Вычесть/1
    вычесть/1
    умножить/1
  |

  test "sort" do
    assert Enum.sort_by(@input, &ExDoc.Utils.NaturalOrder.to_sortable_charlist/1) == @sorted
  end
end
