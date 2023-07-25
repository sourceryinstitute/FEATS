module vertex_m
  !! summary: Represent one node in a directed acyclic graph.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020-2021, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
    implicit none

    private
    public :: vertex_t

    type vertex_t
      !! Encapsulate a node in a graph comprised of vertices connected by dependencies (edges)
      private
      integer, allocatable :: edges_(:)
      character(len=:), allocatable :: label_
      character(len=:), allocatable :: attributes_
    contains
      procedure :: edges
      procedure :: label
      procedure :: attributes
      procedure :: edges_allocated
    end type

    interface vertex_t
      module procedure construct_from_components
    end interface
contains
    pure function construct_from_components(edges, label, attributes) result(vertex)
        !! Component-wise constructor of a vertex_t object
        integer, intent(in) :: edges(:) !! vertices on which this vertex depends
        character(len=*), intent(in) :: label !! vertex description (e.g., name)
        character(len=*), intent(in), optional :: attributes !! Graphvizl .dot symbol description
        type(vertex_t) vertex

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
    end function

      elemental function edges_allocated(self) result(edges_array_allocated)
        !! Result is .true. iff the edges component is allocated
        class(vertex_t), intent(in) :: self
        logical edges_array_allocated

        edges_array_allocated = allocated(self%edges_)
      end function

      pure function edges(self) result(my_edges)
        !! Result is the array element numbers of the vertices on which this vertex depends
        class(vertex_t), intent(in) :: self
        integer :: my_edges(size(self%edges_))

        my_edges = self%edges_
      end function

      pure function label(self) result(my_label)
        !! Vertex label getter
        class(vertex_t), intent(in) :: self
        character(len=:), allocatable :: my_label

        my_label = self%label_
      end function

      pure function attributes(self) result(my_attributes)
        !! Vertex attributes getter
        class(vertex_t), intent(in) :: self
        character(len=:), allocatable :: my_attributes

        my_attributes = self%attributes_
      end function
end module vertex_m
