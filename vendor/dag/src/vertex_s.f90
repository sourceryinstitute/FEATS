submodule(vertex_m) vertex_s
  use iso_varying_string, only: assignment(=)
  implicit none
contains

  module procedure edges_allocated
    edges_array_allocated = allocated(self%edges_)
  end procedure

  module procedure construct_from_components

    character(len=*), parameter :: &
       branch    = 'shape=square, fillcolor="SlateGray1", style=filled' &
      ,external_ = 'shape=square, fillcolor="green",      style=filled' &
      ,root      = 'shape=circle, fillcolor="white",      style=filled' &
      ,leaf      = 'shape=circle, fillcolor="cornsilk",   style=filled'

    vertex%edges_ = edges
    vertex%label_ = label
    if (present(attributes)) then
      vertex%attributes_ = attributes
    else
      vertex%attributes_ = branch
    end if
  end procedure

  module procedure edges
    my_edges = self%edges_
  end procedure

  module procedure label
    my_label = self%label_
  end procedure

  module procedure attributes
    my_attributes = self%attributes_
  end procedure

end submodule vertex_s
