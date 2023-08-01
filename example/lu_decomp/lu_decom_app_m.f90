module lu_decomp_app_m
    use dag_m, only: dag_t
    use iso_fortran_env, only: wp => real64
    use payload_m, only: payload_t
    use task_m, only: task_t
    use vertex_m, only: vertex_t

    implicit none
    private
    public :: generate_application

    type, extends(task_t) :: initial_t
        real(wp), allocatable :: initial_matrix(:,:)
    contains
        procedure :: execute => initial_execute
    end type

    type, extends(task_t) :: calc_factor_t
        integer :: row, step
    contains
        procedure :: execute => calc_factor_execute
    end type

    type, extends(task_t) :: row_multiply_t
        integer :: step
    contains
        procedure :: execute => row_multiply_execute
    end type

    type, extends(task_t) :: row_subtract_t
        integer :: row
    contains
        procedure :: execute => row_subtract_execute
    end type

    type, extends(task_t) :: reconstruct_t
        integer :: step
    contains
        procedure :: execute => reconstruct_execute
    end type

    type, extends(task_t) :: back_substitute_t
        integer :: n_rows
    contains
        procedure :: execute => back_substitute_execute
    end type

    type, extends(task_t) :: print_matrix_t
        integer :: step
    contains
        procedure :: execute => print_matrix_execute
    end type
contains
    function generate_application() result(dag)
        type(dag_t) :: dag

        type(vertex_t), allocatable :: vertices(:)
        real(wp), allocatable :: matrix(:,:)
        integer :: i, matrix_size, step, row, num_tasks, latest_matrix, task_base, reconstruction_step

        call read_matrix_in(matrix, matrix_size)

        num_tasks = 4 + (matrix_size-1)*2 + sum([((matrix_size-step)*3, step=1,matrix_size-1)])
        allocate(vertices(num_tasks))

        vertices(1) = vertex_t([integer::], initial_t(matrix))
        vertices(2) = vertex_t([1], print_matrix_t(0))
        do step = 1, matrix_size-1
            do row = step+1, matrix_size
                latest_matrix = 1 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) ! reconstructed matrix from last step
                task_base = sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) + 3*(row-(step+1))
                vertices(3+task_base) = vertex_t([latest_matrix], calc_factor_t(row=row, step=step))
                vertices(4+task_base) = vertex_t([latest_matrix, 3+task_base], row_multiply_t(step=step))
                vertices(5+task_base) = vertex_t([latest_matrix, 4+task_base], row_subtract_t(row=row))
            end do
            reconstruction_step = 3 + sum([(3*(matrix_size-i), i = 1, step)]) + 2*(step-1)
            vertices(reconstruction_step) = vertex_t( &
                [ 1 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) &
                , [(5 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) + 3*(row-(step+1)) &
                    , row=step+1, matrix_size)] &
                ], &
                reconstruct_t(step=step)) ! depends on previous reconstructed matrix and just subtracted rows
            ! print the just reconstructed matrix
            vertices(reconstruction_step+1) = vertex_t([reconstruction_step], print_matrix_t(step))
        end do
        vertices(num_tasks-1) = vertex_t( &
            [([(3 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) + 3*(row-(step+1)) &
                , row=step+1, matrix_size)] &
                , step=1, matrix_size-1)] &
            , back_substitute_t(n_rows=matrix_size)) ! depends on all "factors"
        vertices(num_tasks) = vertex_t([num_tasks-1], print_matrix_t(matrix_size))

        dag = dag_t(vertices)
    end function

    function initial_execute(self, arguments) result(output)
        class(initial_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        output = package_matrix(self%initial_matrix)
    end function

    function calc_factor_execute(self, arguments) result(output)
        class(calc_factor_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        integer, allocatable :: matrix_data(:)
        integer :: n_row, n_col
        real(wp), allocatable :: matrix(:,:)
        real(wp) :: factor

        matrix = unpack_matrix(arguments(1))
        factor = matrix(self%row, self%step) / matrix(self%step, self%step)
        output = payload_t(transfer(factor, [integer::]))
    end function

    function row_multiply_execute(self, arguments) result(output)
        class(row_multiply_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: matrix(:,:)
        real(wp) :: factor
        real(wp), allocatable :: new_row(:)

        matrix = unpack_matrix(arguments(1))
        factor = transfer(arguments(2)%raw_payload(), factor)
        new_row = factor * matrix(self%step, :)
        output = package_row(new_row)
    end function

    function row_subtract_execute(self, arguments) result(output)
        class(row_subtract_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: matrix(:,:)
        real(wp), allocatable :: row(:)
        real(wp), allocatable :: new_row(:)

        matrix = unpack_matrix(arguments(1))
        row = unpack_row(arguments(2))
        new_row = matrix(self%row, :) - row
        output = package_row(new_row)
    end function

    function reconstruct_execute(self, arguments) result(output)
        class(reconstruct_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: original_matrix(:, :)
        real(wp), allocatable :: new_matrix(:, :)
        integer :: i

        original_matrix = unpack_matrix(arguments(1))
        allocate(new_matrix, mold=original_matrix)
        do i = 1, self%step
            new_matrix(i, :) = original_matrix(i, :)
        end do
        do i = self%step+1, size(original_matrix, dim=1)
            new_matrix(i, :) = unpack_row(arguments(i - self%step + 1))
        end do
        output = package_matrix(new_matrix)
    end function

    function back_substitute_execute(self, arguments) result(output)
        class(back_substitute_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: new_matrix(:,:)
        integer :: row, col, f

        allocate(new_matrix(self%n_rows, self%n_rows))
        do row = 1, self%n_rows
            new_matrix(row, row) = 1
            new_matrix(row, row+1:) = 0
        end do
        f = 1
        do col = 1, self%n_rows-1
            do row = col+1, self%n_rows
                new_matrix(row, col) = transfer(arguments(f)%raw_payload(), 1.0_wp)
                f = f + 1
            end do
        end do
        output = package_matrix(new_matrix)
    end function

    function print_matrix_execute(self, arguments) result(output)
        class(print_matrix_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: matrix(:,:)
        integer :: i

        critical
            print *, ""
            matrix = unpack_matrix(arguments(1))
            print *, "Step: ", self%step
            do i = 1, size(matrix, dim=1)
                print *, matrix(i, :)
            end do
            print *, ""
        end critical
    end function

    subroutine read_matrix_in(matrix, matrix_size)
        real(wp), allocatable, intent(out) :: matrix(:, :)
        integer, intent(out) :: matrix_size

        integer :: arg_len, fu, i, stat
        character(len=:), allocatable :: arg

        if (this_image() == 1) then
            if (command_argument_count() /= 1) error stop "Provide matrix file as command line argument"
            call get_command_argument(number=1, length=arg_len)
            allocate(character(len=arg_len) :: arg)
            call get_command_argument(number=1, value=arg)

            open(file=arg, newunit=fu, status="old")
            matrix_size = 0
            do
                read(fu, *, iostat=stat)
                if (is_iostat_end(stat)) exit
                if (stat /= 0) error stop stat
                matrix_size = matrix_size + 1
            end do
            close(fu)
        end if

        call co_broadcast(matrix_size, 1)
        allocate(matrix(matrix_size, matrix_size))

        if (this_image() == 1) then
            open(file=arg, newunit=fu, status="old")
            do i = 1, matrix_size
                read(fu, *) matrix(:, i)
            end do
            close(fu)
        end if
        call co_broadcast(matrix, 1)
    end subroutine

    pure function package_matrix(matrix) result(payload)
        real(wp), intent(in) :: matrix(:, :)
        type(payload_t) :: payload

        integer :: data_size
        integer, allocatable :: data(:)

        data_size = &
            2 &
            + ceiling(size(matrix) &
            * real(storage_size(matrix)) &
            / real(storage_size(data)))
        allocate(data(data_size))
        data(1) = size(matrix, dim=1)
        data(2) = size(matrix, dim=2)
        data(3:) = transfer(matrix, data)
        payload = payload_t(data)
    end function

    pure function unpack_matrix(payload) result(matrix)
        type(payload_t), intent(in) :: payload
        real(wp), allocatable :: matrix(:, :)

        integer, allocatable :: matrix_data(:)
        integer :: n_row, n_col

        matrix_data = payload%raw_payload()
        n_row = matrix_data(1)
        n_col = matrix_data(2)
        matrix = reshape(transfer(matrix_data(3:), [real(wp)::], n_row*n_col), [n_row, n_col])
    end function

    pure function package_row(row) result(payload)
        real(wp), intent(in) :: row(:)
        type(payload_t) :: payload

        integer :: data_size
        integer, allocatable :: data(:)

        data_size = &
            1 &
            + ceiling(size(row) &
            * real(storage_size(row)) &
            / real(storage_size(data)))
        allocate(data(data_size))
        data(1) = size(row)
        data(2:) = transfer(row, data)
        payload = payload_t(data)
    end function

    pure function unpack_row(payload) result(row)
        type(payload_t), intent(in) :: payload
        real(wp), allocatable :: row(:)

        integer, allocatable :: row_data(:)
        integer :: n_col

        row_data = payload%raw_payload()
        n_col = row_data(1)
        row = transfer(row_data(2:), [real(wp)::], n_col)
    end function
end module
