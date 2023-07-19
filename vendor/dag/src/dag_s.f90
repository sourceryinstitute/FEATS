submodule(dag_m) dag_s
  use assert_m, only : assert
  use rojff, only: &
      fallible_json_member_t, &
      fallible_json_object_t, &
      fallible_json_string_t, &
      fallible_json_value_t, &
      json_array_t, &
      json_element_t
  use iso_fortran_env, only: iostat_end
  use iso_varying_string, only : operator(//), char
  use intrinsic_array_m, only : intrinsic_array_t

  implicit none

  type searched_and_ordered_t
    integer, allocatable, dimension(:) :: s, o
  end type

contains

  module procedure construct_from_components
    dag%vertices = vertices
    dag%order = topological_sort(dag)
    call assert(dag%is_sorted_and_acyclic(), "construct_from_components: dag%is_sorted_and_acyclic()")
  end procedure

  pure function topological_sort(dag) result(order)
    !! Provide array of vertex numbers ordered in a way that respects dependencies
    type(dag_t), intent(in) :: dag
    integer, allocatable :: order(:)

    call assert(all(dag%vertices(:)%edges_allocated()), "dag_s topological_sort: all(dag%vertices(:)%edges_allocated())")

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

      call assert(.not. any(v == d), "depth_first_search: cycle detected", intrinsic_array_t([v,d]))

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
      call assert(order_size == num_vertices, "dag_t%is_sorted_and_acyclic: size(self%vertices) == size(self%order)", &
        intrinsic_array_t([order_size, num_vertices]))

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

  module procedure construct_from_json
    type(fallible_json_value_t) :: maybe_vertices

    maybe_vertices = json_object%get("vertices")
    call assert( &
        .not. maybe_vertices%errors%has_any(), &
        "dag_s construct_from_json: .not. errors%has_any()", &
        char(maybe_vertices%errors%to_string()))

    select type (vertices => maybe_vertices%value_)
    type is (json_array_t)
      dag%vertices = vertex_t(vertices%elements)

    class default
      call assert(.false., "dag%from_json: vertices was not an array", vertices%to_compact_string())
    end select
    dag%order = topological_sort(dag)
  end procedure

  module procedure to_json
    type(fallible_json_object_t) maybe_result

    maybe_result = fallible_json_object_t( &
        [ fallible_json_member_t("vertices", json_array_t(json_element_t(self%vertices%to_json()))) &
        ])
    call assert(.not. maybe_result%errors%has_any(), "dag%to_json: .not. errors%has_any()", char(maybe_result%errors%to_string()))
    json_object = maybe_result%object
  end procedure

  module procedure num_vertices
    num_vertices = size(self%vertices)
  end procedure

  module procedure dependencies_for
    dependency_ids = self%vertices(vertex_id)%edges()
  end procedure

  module procedure depends_on

    call assert(vertex_num>=1 .and. vertex_num<=size(self%vertices), "depends_on: index in bounds")

    allocate(dependencies(0))

    block
      integer v

      do v = 1, size(self%vertices)
        if (any(self%vertices(v)%edges() == vertex_num)) dependencies = [dependencies, v]
      end do
    end block

  end procedure

  module procedure graphviz_digraph

    integer istat

    digraph = generate_digraph(self)

  contains

    elemental function integer_to_string(i) result(s)
      integer,intent(in) :: i
      integer, parameter :: max_number_width = 64
      character(len=max_number_width) :: s
      integer :: istat

      write(s,fmt='(ss,I0)',iostat=istat) i
      if (istat==0) then
          s = trim(adjustl(s))
      else
          s = '***'
      end if
    end function integer_to_string

    pure function generate_digraph(self) result(str)
      !! - Result is the string to write out to a *.dot file. (Called by save_digraph())
      implicit none
      class(dag_t),intent(in) :: self
      character(len=:),allocatable :: str

      character(len=*), parameter :: rankdir = 'RL'
        !! - Rank Direction which are applicable inputs to the -rankdir option on the digraph command
      integer, parameter :: dpi=300
        !! - dots per inch

      integer :: i,j
      integer :: n_edges
      character(len=:),allocatable :: attributes, label

      character(len=*),parameter :: tab = '  '
      character(len=*),parameter :: newline = new_line(' ')


      call assert(all(self%vertices(:)%edges_allocated()), "generate_digraph: self%edges_allocated()")

      str = 'digraph G {'//newline//newline
      str = str//tab//'rankdir='//rankdir//newline//newline
      str = str//tab//'graph [ dpi = '//integer_to_string(dpi)//' ]'//newline//newline

      ! define the vertices:
      do i=1,size(self%vertices)
        label = char('label="'//self%vertices(i)%label()//'"')
        attributes = char('['//self%vertices(i)%attributes()//','//label//']')
        str = str//tab//integer_to_string(i)//' '//attributes//newline
        if (i==size(self%vertices)) str = str//newline
      end do

      ! define the dependencies:
      do i=1,size(self%vertices)
        n_edges = size(self%vertices(i)%edges())
        str = str//tab//integer_to_string(i)//merge(' -> ','    ',n_edges/=0)
        do j=1,n_edges
          ! comma-separated list:
          associate(edges => self%vertices(i)%edges())
            str = str//integer_to_string(edges(j))
            if (n_edges>1 .and. j<n_edges) str = str//','
          end associate
        end do
        str = str//';'//newline
      end do

      str = str//newline//'}'

    end function generate_digraph

  end procedure

end submodule dag_s
