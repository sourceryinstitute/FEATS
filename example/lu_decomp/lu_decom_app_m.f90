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
    contains
        procedure :: execute => print_matrix_execute
    end type
contains
    function generate_application() result(dag)
        type(dag_t) :: dag
        type(vertex_t), allocatable :: vertices(:)

        real(wp), allocatable :: matrix(:,:)

        integer :: matrix_size, step, row, previous_task, latest_matrix
        integer, allocatable :: for_reconstruction(:), for_back_substitution(:)
        integer :: arg_len, fu
        character(len=:), allocatable :: arg

        if (this_image() == 1) then
            call get_command_argument(number=1, length=arg_len)
            allocate(character(len=arg_len) :: arg)
            call get_command_argument(number=1, value=arg)
            read(arg, *) matrix_size
            deallocate(arg)
        end if
        call co_broadcast(matrix_size, 1)
        allocate(matrix(matrix_size, matrix_size))
        if (this_image() == 1) then
            call get_command_argument(number=2, length=arg_len)
            allocate(character(len=arg_len) :: arg)
            call get_command_argument(number=2, value=arg)
            open(file=arg, newunit=fu, status="old")
            deallocate(arg)
            do row = 1, matrix_size
                read(fu, *) matrix(:,row)
            end do
            close(fu)
        end if
        call co_broadcast(matrix, 1)

        previous_task = 0

        vertices = [vertex_t([integer::], initial_t(matrix))]
        previous_task = previous_task + 1
        latest_matrix = previous_task

        vertices = [vertices, vertex_t([latest_matrix], print_matrix_t())]
        previous_task = previous_task + 1

        allocate(for_back_substitution(0))
        do step = 1, matrix_size-1
            for_reconstruction = [latest_matrix]
            do row = step+1, matrix_size
                vertices = [vertices, vertex_t([latest_matrix], calc_factor_t(row=row, step=step))]
                previous_task = previous_task + 1
                for_back_substitution = [for_back_substitution, previous_task]

                vertices = [vertices, vertex_t([latest_matrix, previous_task], row_multiply_t(step=step))]
                previous_task = previous_task + 1

                vertices = [vertices, vertex_t([latest_matrix, previous_task], row_subtract_t(row=row))]
                previous_task = previous_task + 1
                for_reconstruction = [for_reconstruction, previous_task]
            end do
            vertices = [vertices, vertex_t(for_reconstruction, reconstruct_t(step=step))]
            previous_task = previous_task + 1
            latest_matrix = previous_task

            vertices = [vertices, vertex_t([latest_matrix], print_matrix_t())]
            previous_task = previous_task + 1
        end do
        vertices = [vertices, vertex_t(for_back_substitution, back_substitute_t(n_rows=matrix_size))]
        previous_task = previous_task + 1
        deallocate(for_back_substitution)
        latest_matrix = previous_task

        vertices = [vertices, vertex_t([latest_matrix], print_matrix_t())]
        previous_task = previous_task + 1

        dag = dag_t(vertices)
    end function

    function initial_execute(self, arguments) result(output)
        class(initial_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        integer, allocatable :: data(:)
        integer :: data_size

        data_size = &
            2 &
            + ceiling(size(self%initial_matrix) &
            * real(storage_size(self%initial_matrix)) &
            / real(storage_size(data)))
        allocate(data(data_size))
        data(1) = size(self%initial_matrix, dim=1)
        data(2) = size(self%initial_matrix, dim=2)
        data(3:) = transfer(self%initial_matrix, data)
        output = payload_t(data)
    end function

    function calc_factor_execute(self, arguments) result(output)
        class(calc_factor_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: matrix(:,:)
        real(wp) :: factor

        associate(matrix_data => arguments(1)%raw_payload())
            associate(n_row => matrix_data(1), n_col => matrix_data(2))
                matrix = reshape(transfer(matrix_data(3:), matrix, n_row*n_col), [n_row, n_col])
            end associate
        end associate

        factor = matrix(self%row, self%step) / matrix(self%step, self%step)
        output = payload_t(transfer(factor, output%raw_payload()))
    end function

    function row_multiply_execute(self, arguments) result(output)
        class(row_multiply_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: matrix(:,:)
        real(wp) :: factor
        real(wp), allocatable :: new_row(:)
        integer, allocatable :: data(:)
        integer :: data_size

        associate(matrix_data => arguments(1)%raw_payload())
            associate(n_row => matrix_data(1), n_col => matrix_data(2))
                matrix = reshape(transfer(matrix_data(3:), matrix, n_row*n_col), [n_row, n_col])
            end associate
        end associate
        factor = transfer(arguments(2)%raw_payload(), factor)
        new_row = factor * matrix(self%step, :)

        data_size = &
            1 &
            + ceiling(size(new_row) &
            * real(storage_size(new_row)) &
            / real(storage_size(data)))
        allocate(data(data_size))
        data(1) = size(new_row)
        data(2:) = transfer(new_row, data)
        output = payload_t(data)
    end function

    function row_subtract_execute(self, arguments) result(output)
        class(row_subtract_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: matrix(:,:)
        real(wp), allocatable :: row(:)
        real(wp), allocatable :: new_row(:)
        integer, allocatable :: data(:)
        integer :: data_size

        associate(matrix_data => arguments(1)%raw_payload())
            associate(n_row => matrix_data(1), n_col => matrix_data(2))
                matrix = reshape(transfer(matrix_data(3:), matrix, n_row*n_col), [n_row, n_col])
            end associate
        end associate
        associate(row_data => arguments(2)%raw_payload())
            associate(n_cols => row_data(1))
                row = transfer(row_data(2:), row, n_cols)
            end associate
        end associate

        new_row = matrix(self%row, :) - row

        data_size = &
            1 &
            + ceiling(size(new_row) &
            * real(storage_size(new_row)) &
            / real(storage_size(data)))
        allocate(data(data_size))
        data(1) = size(new_row)
        data(2:) = transfer(new_row, data)
        output = payload_t(data)
    end function

    function reconstruct_execute(self, arguments) result(output)
        class(reconstruct_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: original_matrix(:, :)
        real(wp), allocatable :: new_matrix(:, :)
        integer :: i
        integer, allocatable :: data(:)
        integer :: data_size

        associate(matrix_data => arguments(1)%raw_payload())
            associate(n_row => matrix_data(1), n_col => matrix_data(2))
                original_matrix = reshape(transfer(matrix_data(3:), original_matrix, n_row*n_col), [n_row, n_col])
            end associate
        end associate
        allocate(new_matrix, mold=original_matrix)
        do i = 1, self%step
            new_matrix(i, :) = original_matrix(i, :)
        end do
        do i = self%step+1, size(original_matrix, dim=1)
            associate(row_data => arguments(i - self%step + 1)%raw_payload())
                associate(n_cols => row_data(1))
                    new_matrix(i, :) = transfer(row_data(2:), new_matrix, n_cols)
                end associate
            end associate
        end do

        data_size = &
            2 &
            + ceiling(size(new_matrix) &
            * real(storage_size(new_matrix)) &
            / real(storage_size(data)))
        allocate(data(data_size))
        data(1) = size(new_matrix, dim=1)
        data(2) = size(new_matrix, dim=2)
        data(3:) = transfer(new_matrix, data)
        output = payload_t(data)
    end function

    function back_substitute_execute(self, arguments) result(output)
        class(back_substitute_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: new_matrix(:,:)
        integer :: row, col, f
        integer, allocatable :: data(:)
        integer :: data_size

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

        data_size = &
            2 &
            + ceiling(size(new_matrix) &
            * real(storage_size(new_matrix)) &
            / real(storage_size(data)))
        allocate(data(data_size))
        data(1) = size(new_matrix, dim=1)
        data(2) = size(new_matrix, dim=2)
        data(3:) = transfer(new_matrix, data)
        output = payload_t(data)
    end function

    function print_matrix_execute(self, arguments) result(output)
        class(print_matrix_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real(wp), allocatable :: matrix(:,:)
        integer :: i

        print *, ""
        associate(matrix_data => arguments(1)%raw_payload())
            associate(n_row => matrix_data(1), n_col => matrix_data(2))
                matrix = reshape(transfer(matrix_data(3:), matrix, n_row*n_col), [n_row, n_col])
                do i = 1, n_row
                    print *, matrix(i, :)
                end do
            end associate
        end associate
        print *, ""
    end function
end module
