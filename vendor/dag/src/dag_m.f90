module dag_m
  !! summary: A directed acyclic graph (DAG) abstraction.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
  use vertex_m, only : vertex_t
  use rojff, only : json_object_t

  implicit none

  private

  type,public :: dag_t
    !! Encapsulate a graph as an array of vertices, each storing dependency information
    private
    type(vertex_t), allocatable :: vertices(:)
    integer, allocatable :: order(:)
  contains
    procedure :: is_sorted_and_acyclic
    procedure :: to_json
    procedure :: num_vertices
    procedure :: dependencies_for
    procedure :: depends_on
    procedure :: graphviz_digraph
  end type

  interface dag_t

    module function construct_from_json(json_object) result(dag)
      !! Construct a dag_t object from a JSON object (result contains a topologically sorted index array)
      implicit none
      type(json_object_t), intent(in) :: json_object
      type(dag_t) dag
    end function

    pure module function construct_from_components(vertices) result(dag)
      !! Construct a dag_t object from an array of (unsorted) vertex_t objects (result contains a topologically sorted index array)
      implicit none
      type(vertex_t), intent(in) :: vertices(:)
      type(dag_t) dag
    end function

  end interface

  interface

    elemental module function is_sorted_and_acyclic(self)
      !! Result is true if dag%order contains a topological sorting of vertex identifiers
      implicit none
      class(dag_t), intent(in) :: self
      logical is_sorted_and_acyclic
    end function

    module function to_json(self) result(json_object)
      !! Result is a JSON representation of the dag_t object
      implicit none
      class(dag_t), intent(in) :: self
      type(json_object_t) json_object
    end function

    elemental module function num_vertices(self)
      !! Result is the size of the vertex array
      implicit none
      class(dag_t), intent(in) :: self
      integer num_vertices
    end function

    pure module function depends_on(self, vertex_num) result(dependencies)
      !! Result is an array of the vertex numbers that depend on on vertex vertex_num
      implicit none
      class(dag_t), intent(in) :: self
      integer, intent(in) :: vertex_num
      integer, allocatable :: dependencies(:)
    end function

    pure module function dependencies_for(self, vertex_id) result(dependency_ids)
      !! Result is an array of the ids on which vertex_id depends
      implicit none
      class(dag_t), intent(in) :: self
      integer, intent(in) :: vertex_id
      integer, allocatable :: dependency_ids(:)
    end function

    pure module function graphviz_digraph(self) result(digraph)
      !! Result contains a Graphviz .dot file descriptoin of the dag_t object
      implicit none
      class(dag_t),intent(in) :: self
      character(len=:), allocatable :: digraph
    end function

  end interface

end module dag_m
