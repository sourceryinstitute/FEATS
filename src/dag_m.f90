module dag_m
  !! summary: A directed acyclic graph (DAG) abstraction.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
  use iso_fortran_env, only: iostat_end
  use vertex_m, only : vertex_t

  implicit none

  private
  public :: dag_t

  type :: dag_t
    !! Encapsulate a graph as an array of vertices, each storing dependency information
    type(vertex_t), allocatable :: vertices(:)
  contains
    procedure :: dependencies_for
    procedure :: depends_on
  end type

  interface dag_t
    module procedure construct_from_vertices
  end interface
contains
    function construct_from_vertices(vertices) result(dag)
      !! Construct a dag_t object from an array of (unsorted) vertex_t objects (result contains a topologically sorted index array)
      type(vertex_t), intent(in) :: vertices(:)
      type(dag_t) dag

      dag%vertices = vertices
    end function

    pure function depends_on(self, vertex_num) result(dependencies)
      !! Result is an array of the vertex numbers that depend on on vertex vertex_num
      class(dag_t), intent(in) :: self
      integer, intent(in) :: vertex_num
      integer, allocatable :: dependencies(:)

      allocate(dependencies(0))

      block
        integer v

        do v = 1, size(self%vertices)
          if (any(self%vertices(v)%edges == vertex_num)) dependencies = [dependencies, v]
        end do
      end block
    end function

    pure function dependencies_for(self, vertex_id) result(dependency_ids)
      !! Result is an array of the ids on which vertex_id depends
      class(dag_t), intent(in) :: self
      integer, intent(in) :: vertex_id
      integer, allocatable :: dependency_ids(:)

      dependency_ids = self%vertices(vertex_id)%edges
    end function
end module dag_m
