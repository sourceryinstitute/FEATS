submodule(dag_m) dag_s
  use iso_fortran_env, only: iostat_end

  implicit none

  type searched_and_ordered_t
    integer, allocatable, dimension(:) :: s, o
  end type

contains

  module procedure construct_from_components
    dag%vertices = vertices
    dag%order = topological_sort(dag)
  end procedure

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
          searched_and_ordered = depth_first_search(v, [integer::], searched_and_ordered%o)
      end do
      order = searched_and_ordered%o
    end block

  contains

    pure recursive function depth_first_search(v, d, o) result(hybrid)
      integer, intent(in) :: v
      integer, intent(in), dimension(:) :: d, o
      type(searched_and_ordered_t) hybrid

      hybrid = searched_and_ordered_t(s = [integer::], o = o)

      associate(dependencies => dag%depends_on(v))
        block
          integer w
          do w = 1, size(dependencies)
            associate(w_dependencies => dependencies(w))
              if (.not. any(w_dependencies == hybrid%s)) hybrid = depth_first_search(w_dependencies, [d, v], hybrid%o)
            end associate
          end do
        end block
      end associate

      if (.not. any(v == hybrid%o)) hybrid%o = [v, hybrid%o]
      hybrid = searched_and_ordered_t(s = [v, hybrid%s], o = hybrid%o)

    end function

  end function topological_sort

  module procedure is_sorted_and_acyclic

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

  end procedure

  module procedure num_vertices
    num_vertices = size(self%vertices)
  end procedure

  module procedure dependencies_for
    dependency_ids = self%vertices(vertex_id)%edges()
  end procedure

  module procedure depends_on

    allocate(dependencies(0))

    block
      integer v

      do v = 1, size(self%vertices)
        if (any(self%vertices(v)%edges() == vertex_num)) dependencies = [dependencies, v]
      end do
    end block

  end procedure

end submodule dag_s
