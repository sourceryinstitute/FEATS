module quadratic_solver_application_generator_m
    use dag_m, only: dag_t
    use iso_fortran_env, only: input_unit, output_unit
    use legacy_m, only: square, four_a_c
    use payload_m, only: payload_t
    use task_m, only: task_t
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
    contains
        procedure :: execute => b_squared_execute
    end type

    type, extends(task_t) :: four_ac_t
    contains
        procedure :: execute => four_ac_execute
    end type

    type, extends(task_t) :: square_root_t
    contains
        procedure :: execute => square_root_execute
    end type

    type, extends(task_t) :: minus_b_pm_square_root_t
    contains
      procedure :: execute => minus_b_pm_square_root_execute
    end type

    type, extends(task_t) :: two_a_t
    contains
      procedure :: execute => two_a_execute
    end type

    type, extends(task_t) :: division_t
    contains
      procedure :: execute => division_execute
    end type

    type, extends(task_t) :: printer_t
    contains
      procedure :: execute => printer_execute
    end type
contains
    function generate_application() result(solver)
      type(dag_t) solver

      real :: a, b, c
      if (this_image() == 1) then
        write(output_unit, "(A)") "Enter values for a, b and c in `a*x**2 + b*x + c`:"
        flush(output_unit)
        read(input_unit, *) a, b, c
      end if
      call co_broadcast(a, 1)
      call co_broadcast(b, 1)
      call co_broadcast(c, 1)

      solver = &
          dag_t([ &
                vertex_t([integer::], a_t(a)) &
              , vertex_t([integer::], b_t(b)) &
              , vertex_t([integer::], c_t(c)) &
              , vertex_t([2], b_squared_t()) &
              , vertex_t([1, 3], four_ac_t()) &
              , vertex_t([4, 5], square_root_t()) &
              , vertex_t([2, 6], minus_b_pm_square_root_t()) &
              , vertex_t([1], two_a_t()) &
              , vertex_t([8, 7], division_t()) &
              , vertex_t([9], printer_t()) &
          ])
    end function

    function a_execute(self, arguments) result(output)
        class(a_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        print *, "a = ", self%a
        output = payload_t(transfer(self%a, [integer::]))
    end function

    function b_execute(self, arguments) result(output)
        class(b_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        print *, "b = ", self%b
        output = payload_t(transfer(self%b, [integer::]))
    end function

    function c_execute(self, arguments) result(output)
        class(c_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        print *, "c = ", self%c
        output = payload_t(transfer(self%c, [integer::]))
    end function

    function b_squared_execute(self, arguments) result(output)
        class(b_squared_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real :: b, b_squared

        b = transfer(arguments(1)%raw_payload(), b)
        b_squared = square(b)
        print *, "b**2 = ", b_squared
        output = payload_t(transfer(b_squared, [integer::]))
    end function

    function four_ac_execute(self, arguments) result(output)
      class(four_ac_t), intent(in) :: self
      type(payload_t), intent(in) :: arguments(:)
      type(payload_t) :: output

      real :: a, c, four_a_c

      a = transfer(arguments(1)%raw_payload(), a)
      c = transfer(arguments(2)%raw_payload(), c)
      four_a_c = 4*a*c
      print *, "4*a*c = ", four_a_c
      output = payload_t(transfer(four_a_c, [integer::]))
    end function

    function square_root_execute(self, arguments) result(output)
      class(square_root_t), intent(in) :: self
      type(payload_t), intent(in) :: arguments(:)
      type(payload_t) :: output

      real :: b_squared, four_a_c, square_roots(2), square

        b_squared = transfer(arguments(1)%raw_payload(), b_squared)
        four_a_c = transfer(arguments(2)%raw_payload(), four_a_c)

        square = b_squared - four_a_c
        square_roots = [sqrt(square), -sqrt(square)]
        print *, "sqrt(b**2 - 4*a*c) = ", square_roots
        output = payload_t(transfer(square_roots, [integer::]))
    end function

    function minus_b_pm_square_root_execute(self, arguments) result(output)
      class(minus_b_pm_square_root_t), intent(in) :: self
      type(payload_t), intent(in) :: arguments(:)
      type(payload_t) :: output

      real :: b, square_root(2), minus_b_pm_roots(2)

        b = transfer(arguments(1)%raw_payload(), b)
        square_root = transfer(arguments(2)%raw_payload(), square_root)
        minus_b_pm_roots = -b + square_root
        print *, "-b +- sqrt(b**2 - 4*a*c) = ", minus_b_pm_roots
        output = payload_t(transfer(minus_b_pm_roots, [integer::]))
    end function

    function two_a_execute(self, arguments) result(output)
      class(two_a_t), intent(in) :: self
      type(payload_t), intent(in) :: arguments(:)
      type(payload_t) :: output

      real :: a, two_a

      a = transfer(arguments(1)%raw_payload(), a)
      two_a = 2*a
      print *, "2*a = ", two_a
      output = payload_t(transfer(two_a, [integer::]))
    end function

    function division_execute(self, arguments) result(output)
      class(division_t), intent(in) :: self
      type(payload_t), intent(in) :: arguments(:)
      type(payload_t) :: output

      real :: two_a, b_pm_square_root(2), quotients(2)

        two_a = transfer(arguments(1)%raw_payload(), two_a)
        b_pm_square_root = transfer(arguments(2)%raw_payload(), b_pm_square_root)
        quotients = b_pm_square_root / two_a
        print *, "(-b +- sqrt(b**2 - 4*a*c)) / (2*a) = ", quotients
        output = payload_t(transfer(quotients, [integer::]))
    end function

    function printer_execute(self, arguments) result(output)
      class(printer_t), intent(in) :: self
      type(payload_t), intent(in) :: arguments(:)
      type(payload_t) :: output

      real :: answers(2)

      answers = transfer(arguments(1)%raw_payload(), answers)
      print *, "The roots are ", answers
      output = payload_t()
    end function
end module
