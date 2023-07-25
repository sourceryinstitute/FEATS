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
    private
    type(vertex_t), allocatable :: vertices(:)
    integer, allocatable :: order(:)
  contains
    procedure :: is_sorted_and_acyclic
    procedure :: num_vertices
    procedure :: dependencies_for
    procedure :: depends_on
  end type

  type searched_and_ordered_t
    integer, allocatable, dimension(:) :: s, o
  end type

  interface dag_t
    module procedure construct_from_components
  end interface
contains
    pure function construct_from_components(vertices) result(dag)
      !! Construct a dag_t object from an array of (unsorted) vertex_t objects (result contains a topologically sorted index array)
      type(vertex_t), intent(in) :: vertices(:)
      type(dag_t) dag

      dag%vertices = vertices
      dag%order = topological_sort(dag)
    end function

    elemental function is_sorted_and_acyclic(self)
      !! Result is true if dag%order contains a topological sorting of vertex identifiers
      class(dag_t), intent(in) :: self
      logical is_sorted_and_acyclic

      if (.not. allocated(self%order)) then
        is_sorted_and_acyclic = .false.
        return
      end if

      associate(num_vertices => size(self%vertices), order_size => size(self%order))

        block
          integer i, j

          do i = 1, num_vertices
            associate(edges => self%vertices(self%order(i))%edges())
              do j = 1, size(edges)
                if (.not. any(edges(j) == self%order(1:i))) then
                  is_sorted_and_acyclic = .false.
                  return
                end if
              end do
            end associate
          end do

          is_sorted_and_acyclic = .true.
        end block

      end associate
    end function

    elemental function num_vertices(self)
      !! Result is the size of the vertex array
      class(dag_t), intent(in) :: self
      integer num_vertices

      num_vertices = size(self%vertices)
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
          if (any(self%vertices(v)%edges() == vertex_num)) dependencies = [dependencies, v]
        end do
      end block
    end function

    pure function dependencies_for(self, vertex_id) result(dependency_ids)
      !! Result is an array of the ids on which vertex_id depends
      class(dag_t), intent(in) :: self
      integer, intent(in) :: vertex_id
      integer, allocatable :: dependency_ids(:)

      dependency_ids = self%vertices(vertex_id)%edges()
    end function

    pure function topological_sort(dag) result(order)
      !! Provide array of vertex numbers ordered in a way that respects dependencies
      type(dag_t), intent(in) :: dag
      integer, allocatable :: order(:)

      block
        type(searched_and_ordered_t) searched_and_ordered
        integer v

        searched_and_ordered = searched_and_ordered_t(s = [integer::], o = [integer::])

        do v = 1, size(dag%vertices)
          if (.not. any(v == searched_and_ordered%s)) &
            searched_and_ordered = depth_first_search(dag, v, [integer::], searched_and_ordered%o)
        end do
        order = searched_and_ordered%o
      end block

    end function topological_sort

      pure recursive function depth_first_search(dag, v, d, o) result(hybrid)
        type(dag_t), intent(in) :: dag
        integer, intent(in) :: v
        integer, intent(in), dimension(:) :: d, o
        type(searched_and_ordered_t) hybrid

        hybrid = searched_and_ordered_t(s = [integer::], o = o)

        associate(dependencies => dag%depends_on(v))
          block
            integer w
            do w = 1, size(dependencies)
              associate(w_dependencies => dependencies(w))
                if (.not. any(w_dependencies == hybrid%s)) hybrid = depth_first_search(dag, w_dependencies, [d, v], hybrid%o)
              end associate
            end do
          end block
        end associate

        if (.not. any(v == hybrid%o)) hybrid%o = [v, hybrid%o]
        hybrid = searched_and_ordered_t(s = [v, hybrid%s], o = hybrid%o)

      end function

end module dag_m
