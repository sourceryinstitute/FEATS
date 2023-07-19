submodule(vertex_m) vertex_s
  use rojff, only : &
      fallible_json_member_t, &
      fallible_json_object_t, &
      fallible_json_string_t, &
      fallible_json_value_t, &
      json_array_t, &
      json_number_t, &
      json_string_t, &
      json_integer_t
  use iso_varying_string, only : assignment(=), char
  use assert_m, only : assert
  implicit none

contains

  module procedure edges_allocated
    edges_array_allocated = allocated(self%edges_)
  end procedure

  module procedure to_json
    type(fallible_json_object_t) :: maybe_result

    maybe_result = fallible_json_object_t( &
        [ fallible_json_member_t("label", fallible_json_value_t(fallible_json_string_t(self%label_))) &
        , fallible_json_member_t("edges", json_array_t(json_element_t(json_integer_t(self%edges_)))) &
        ])
    call assert( &
        .not.  maybe_result%errors%has_any(), &
        "vertex%to_json: .not. errors%has_any()", &
        char( maybe_result%errors%to_string()))
    json_object = maybe_result%object
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

  module procedure from_json_element
    vertex = vertex_t(json_element%json)
  end procedure

  module procedure from_json_value
    select type (json_value)
    type is (json_object_t)
      vertex = from_json_object(json_value)
    class default
      call assert(.false., "vertex%from_json_value: vertex was not an object", json_value%to_compact_string())
    end select
  end procedure

  module procedure from_json_object
    type(fallible_json_value_t) :: maybe_edges
    integer :: i

    maybe_edges = json_object%get("edges")
    call assert( &
        .not. maybe_edges%errors%has_any(), &
        "vertex%from_json: .not. errors%has_any()", &
        char(maybe_edges%errors%to_string()))
    select type (edges => maybe_edges%value_)
    type is (json_array_t)
      allocate(vertex%edges_(size(edges%elements)))
      do i = 1, size(edges%elements)
        select type (edge => edges%elements(i)%json)
        type is (json_number_t)
          vertex%edges_(i) = int(edge%number)
        type is (json_integer_t)
          vertex%edges_(i) = edge%number
        class default
          call assert(.false., "vertex%from_json: edge was not a number", edge%to_compact_string())
        end select
      end do
    class default
      call assert(.false., "vertex%from_json: edges was not an array", edges%to_compact_string())
    end select
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
