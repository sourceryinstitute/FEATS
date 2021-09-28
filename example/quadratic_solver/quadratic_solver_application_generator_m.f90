module quadratic_solver_application_generator_m
    use application_m, only: application_t
    use dag_m, only: dag_t
    use iso_varying_string, only: varying_string, var_str
    use legacy_m, only: square, four_a_c
    use payload_m, only: payload_t
    use task_m, only: task_t
    use task_item_m, only: task_item_t
    use task_payload_map_m, only: task_payload_map_t
    use vertex_m, only: vertex_t

    implicit none
    private
    public :: generate_application

    type, extends(task_t) :: a_t
        private
        real :: a
    contains
        procedure :: execute => a_execute
    end type

    type, extends(task_t) :: b_t
        private
        real :: b
    contains
        procedure :: execute => b_execute
    end type

    type, extends(task_t) :: c_t
        private
        real :: c
    contains
        procedure :: execute => c_execute
    end type

    type, extends(task_t) :: b_squared_t
        private
        integer :: b_task_id
    contains
        procedure :: execute => b_squared_execute
    end type

    type, extends(task_t) :: four_ac_t
        private
        integer :: a_task_id, c_task_id
    contains
        procedure :: execute => four_ac_execute
    end type

    type, extends(task_t) :: square_root_t
      private
      integer :: b_squared_task_id, four_a_c_task_id
    contains
        procedure :: execute => square_root_execute
    end type

    type, extends(task_t) :: minus_b_pm_square_root_t
      private
      integer :: b_task_id, square_root_task_id
    contains
      procedure :: execute => minus_b_pm_square_root_execute
    end type

    type, extends(task_t) :: two_a_t
      private
      integer :: a_task_id
    contains
      procedure :: execute => two_a_execute
    end type

    type, extends(task_t) :: division_t
      private
      integer :: two_a_task_id, minus_b_pm_square_root_task_id
    contains
      procedure :: execute => division_execute
    end type

    type, extends(task_t) :: printer_t
      private
      integer :: division_task_id
    contains
      procedure :: execute => printer_execute
    end type
contains
    function generate_application() result(application)
        type(application_t) :: application

        character(len=*), parameter :: longest_name = "minus_b_pm_square_root"
        character(len=len(longest_name)), parameter :: names(*) = &
            [ character(len=len(longest_name)) :: "a" &
            , "b" &
            , "c" &
            , "b_squared" &
            , "four_ac" &
            , "square_root" &
            , "minus_b_pm_square_root" &
            , "two_a" &
            , "division" &
            , "print" &
            ]
        associate( &
              a => findloc(names, "a", dim=1) &
            , b => findloc(names, "b", dim=1) &
            , c => findloc(names, "c", dim=1) &
            , b_squared => findloc(names, "b_squared", dim=1) &
            , four_ac => findloc(names, "four_ac", dim=1) &
            , square_root => findloc(names, "square_root", dim=1) &
            , minus_b_pm_square_root => findloc(names, "minus_b_pm_square_root", dim=1) &
            , two_a => findloc(names, "two_a", dim=1) &
            , division => findloc(names, "division", dim=1) &
            , print => findloc(names, "print", dim=1) &
        )
          block
              character(len=*),           parameter :: root      = 'shape=circle,fillcolor="white",style=filled'
              character(len=*),           parameter :: branch    = 'shape=square,fillcolor="SlateGray1",style=filled'
              character(len=len(branch)), parameter :: leaf      = 'shape=circle,fillcolor="cornsilk",style=filled'
              type(dag_t) solver
              integer i
              type(varying_string) name_string(size(names))
              type(task_item_t), allocatable :: tasks(:)

              name_string = var_str(names)

              solver = &
                  dag_t([ &
                        vertex_t([integer::], name_string(a), var_str(leaf)) &
                      , vertex_t([integer::], name_string(b), var_str(leaf)) &
                      , vertex_t([integer::], name_string(c), var_str(leaf)) &
                      , vertex_t([b], name_string(b_squared), var_str(branch)) &
                      , vertex_t([a, c], name_string(four_ac), var_str(branch)) &
                      , vertex_t([b_squared, four_ac], name_string(square_root), var_str(branch)) &
                      , vertex_t([b, square_root], name_string(minus_b_pm_square_root), var_str(branch)) &
                      , vertex_t([a], name_string(two_a), var_str(branch)) &
                      , vertex_t([minus_b_pm_square_root, two_a], name_string(division), var_str(branch)) &
                      , vertex_t([division], name_string(print), var_str(root)) &
                  ])
              tasks = &
              [ task_item_t(a_t(2.0)) &
              , task_item_t(b_t(-5.0)) &
              , task_item_t(c_t(1.0)) &
              , task_item_t(b_squared_t(b_task_id = b)) &
              , task_item_t(four_ac_t(a_task_id = a, c_task_id = c)) &
              , task_item_t(square_root_t(b_squared_task_id = b_squared, four_a_c_task_id = four_ac)) &
              , task_item_t(minus_b_pm_square_root_t(b_task_id = b, square_root_task_id = square_root)) &
              , task_item_t(two_a_t(a_task_id = a)) &
              , task_item_t(division_t(minus_b_pm_square_root_task_id = minus_b_pm_square_root, two_a_task_id = two_a)) &
              , task_item_t(printer_t(division_task_id = division)) &
              ]
              application = application_t(solver, tasks)
          end block
        end associate
    end function

    function a_execute(self, task_number, upstream_task_results) result(output)
        class(a_t), intent(in) :: self
        integer, intent(in) :: task_number
        type(task_payload_map_t), intent(in) :: upstream_task_results
        type(payload_t) :: output

        output = payload_t(transfer(self%a, output%raw_payload()))
    end function

    function b_execute(self, task_number, upstream_task_results) result(output)
        class(b_t), intent(in) :: self
        integer, intent(in) :: task_number
        type(task_payload_map_t), intent(in) :: upstream_task_results
        type(payload_t) :: output

        output = payload_t(transfer(self%b, output%raw_payload()))
    end function

    function c_execute(self, task_number, upstream_task_results) result(output)
        class(c_t), intent(in) :: self
        integer, intent(in) :: task_number
        type(task_payload_map_t), intent(in) :: upstream_task_results
        type(payload_t) :: output

        output = payload_t(transfer(self%c, output%raw_payload()))
    end function

    function b_squared_execute(self, task_number, upstream_task_results) result(output)
        class(b_squared_t), intent(in) :: self
        integer, intent(in) :: task_number
        type(task_payload_map_t), intent(in) :: upstream_task_results
        type(payload_t) :: output

        character(len=1), allocatable :: b_payload(:)
        real :: b

        call upstream_task_results%get_raw_payload(self%b_task_id, b_payload)
        b = transfer(b_payload, b)
        output = payload_t(transfer(square(b), output%raw_payload()))
    end function

    function four_ac_execute(self, task_number, upstream_task_results) result(output)
      class(four_ac_t), intent(in) :: self
      integer, intent(in) :: task_number
      type(task_payload_map_t), intent(in) :: upstream_task_results
      type(payload_t) :: output

      character(len=1), allocatable :: a_payload(:), c_payload(:)
      real :: a, c

      call upstream_task_results%get_raw_payload(self%a_task_id, a_payload)
      call upstream_task_results%get_raw_payload(self%c_task_id, c_payload)
      a = transfer(a_payload, a)
      c = transfer(c_payload, c)
      output = payload_t(transfer(4*a*c, output%raw_payload()))
    end function

    function square_root_execute(self, task_number, upstream_task_results) result(output)
      class(square_root_t), intent(in) :: self
      integer, intent(in) :: task_number
      type(task_payload_map_t), intent(in) :: upstream_task_results
      type(payload_t) :: output

      character(len=1), allocatable :: b_squared_payload(:), four_ac_payload(:)
      real :: b_squared, four_a_c

        call upstream_task_results%get_raw_payload(self%b_squared_task_id, b_squared_payload)
        call upstream_task_results%get_raw_payload(self%four_a_c_task_id, four_ac_payload)
        b_squared = transfer(b_squared_payload, b_squared)
        four_a_c = transfer(four_ac_payload, four_a_c)
        associate(discriminant => b_squared - four_a_c)
          output = payload_t(transfer([sqrt(discriminant), -sqrt(discriminant)], output%raw_payload()))
        end associate
    end function

    function minus_b_pm_square_root_execute(self, task_number, upstream_task_results) result(output)
      class(minus_b_pm_square_root_t), intent(in) :: self
      integer, intent(in) :: task_number
      type(task_payload_map_t), intent(in) :: upstream_task_results
      type(payload_t) :: output

      character(len=1), allocatable :: b_payload(:), square_root_payload(:)
      real :: b, square_root(2)

        call upstream_task_results%get_raw_payload(self%b_task_id, b_payload)
        call upstream_task_results%get_raw_payload(self%square_root_task_id, square_root_payload)
        b = transfer(b_payload, b)
        square_root = transfer(square_root_payload, square_root)

        output = payload_t(transfer(-b + square_root, output%raw_payload()))
    end function

    function two_a_execute(self, task_number, upstream_task_results) result(output)
      class(two_a_t), intent(in) :: self
      integer, intent(in) :: task_number
      type(task_payload_map_t), intent(in) :: upstream_task_results
      type(payload_t) :: output

      character(len=1), allocatable :: a_payload(:)
      real :: a

      call upstream_task_results%get_raw_payload(self%a_task_id, a_payload)
      a = transfer(a_payload, a)
      output = payload_t(transfer(2*a, output%raw_payload()))
    end function

    function division_execute(self, task_number, upstream_task_results) result(output)
      class(division_t), intent(in) :: self
      integer, intent(in) :: task_number
      type(task_payload_map_t), intent(in) :: upstream_task_results
      type(payload_t) :: output

      character(len=1), allocatable :: two_a_payload(:), b_pm_square_root_payload(:)
      real :: two_a, b_pm_square_root(2)

        call upstream_task_results%get_raw_payload(self%two_a_task_id, two_a_payload)
        call upstream_task_results%get_raw_payload(self%minus_b_pm_square_root_task_id, b_pm_square_root_payload)
        two_a = transfer(two_a_payload, two_a)
        b_pm_square_root = transfer(b_pm_square_root_payload, b_pm_square_root)
        output = payload_t(transfer(b_pm_square_root / two_a, output%raw_payload()))
    end function

    function printer_execute(self, task_number, upstream_task_results) result(output)
      class(printer_t), intent(in) :: self
      integer, intent(in) :: task_number
      type(task_payload_map_t), intent(in) :: upstream_task_results
      type(payload_t) :: output

      character(len=1), allocatable :: answers_payload(:)
      real :: answers(2)

      call upstream_task_results%get_raw_payload(self%division_task_id, answers_payload)
      answers = transfer(answers_payload, answers)
      print *, answers
    end function
end module
