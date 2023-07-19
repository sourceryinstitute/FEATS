module vertex_m
  !! summary: Represent one node in a directed acyclic graph.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020-2021, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
    use iso_varying_string, only : varying_string

    implicit none

    private
    public :: vertex_t

    type vertex_t
      !! Encapsulate a node in a graph comprised of vertices connected by dependencies (edges)
      private
      integer, allocatable :: edges_(:)
      type(varying_string) :: label_
      type(varying_string) :: attributes_
    contains
      procedure :: edges
      procedure :: label
      procedure :: attributes
      procedure :: edges_allocated
    end type

    interface vertex_t

      pure module function construct_from_components(edges, label, attributes) result(vertex)
        !! Component-wise constructor of a vertex_t object
        implicit none
        integer, intent(in) :: edges(:) !! vertices on which this vertex depends
        type(varying_string), intent(in) :: label !! vertex description (e.g., name)
        type(varying_string), intent(in), optional :: attributes !! Graphvizl .dot symbol description
        type(vertex_t) vertex
      end function

    end interface

    interface

      elemental module function edges_allocated(self) result(edges_array_allocated)
        !! Result is .true. iff the edges component is allocated
        implicit none
        class(vertex_t), intent(in) :: self
        logical edges_array_allocated
      end function

      pure module function edges(self) result(my_edges)
        !! Result is the array element numbers of the vertices on which this vertex depends
        implicit none
        class(vertex_t), intent(in) :: self
        integer :: my_edges(size(self%edges_))
      end function

      elemental module function label(self) result(my_label)
        !! Vertex label getter
        implicit none
        class(vertex_t), intent(in) :: self
        type(varying_string) my_label
      end function

      elemental module function attributes(self) result(my_attributes)
        !! Vertex attributes getter
        implicit none
        class(vertex_t), intent(in) :: self
        type(varying_string) my_attributes
      end function

    end interface

end module vertex_m
