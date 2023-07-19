module vertex_m
  !! summary: Represent one node in a directed acyclic graph.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020-2021, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
    use rojff, only : json_element_t, json_object_t, json_value_t
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
      procedure :: to_json
      procedure :: edges
      procedure :: label
      procedure :: attributes
      procedure :: edges_allocated
    end type

    interface vertex_t

      impure elemental module function from_json_element(json_element) result(vertex)
        !! Construct a scalar array vertex_t or an array of objects from a JSON representation of a vertex or vertices
        implicit none
        type(json_element_t), intent(in) :: json_element
        type(vertex_t) :: vertex
      end function

      module function from_json_value(json_value) result(vertex)
        !! Construct a single vertex_t object from a JSON representation of a vertex
        implicit none
        class(json_value_t), intent(in) :: json_value
        type(vertex_t) :: vertex
      end function

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

      module function from_json_object(json_object) result(vertex)
        !! construct a vertexa_t object from a jsonff JSON object
        implicit none
        type(json_object_t), intent(in) :: json_object
        type(vertex_t) :: vertex
      end function

      impure elemental module function to_json(self) result(json_object)
        !! Result is a JSON representation of self
        implicit none
        class(vertex_t), intent(in) :: self
        type(json_object_t) :: json_object
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
