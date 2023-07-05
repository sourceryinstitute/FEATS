module lu_decomp_app_m
    use application_m, only: application_t
    use dag_m, only: dag_t
    use iso_varying_string, only: varying_string, trim, var_str
    use payload_m, only: payload_t
    use task_m, only: task_t
    use task_item_m, only: task_item_t
    use vertex_m, only: vertex_t

    implicit none
    private
    public :: generate_application

    type, extends(task_t) :: initial_t
        real, allocatable :: initial_matrix(:,:)
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

    type, extends(task_t) :: print_matrix_t
    contains
        procedure :: execute => print_matrix_execute
    end type
contains
    function generate_application() result(application)
        type(application_t) :: application

        type(dag_t) :: dag
        type(task_item_t), allocatable :: tasks(:)
        real :: matrix(3,3)

        matrix(1,:) = [25, 5, 1]
        matrix(2,:) = [64, 8, 1]
        matrix(3,:) = [144, 12, 1]

        dag = dag_t( &
            [ vertex_t([integer::], var_str("initial")) &       ! 1
            , vertex_t([1], var_str("factor")) &                ! 2
            , vertex_t([1], var_str("factor")) &                ! 3
            , vertex_t([1, 2], var_str("row_multiply")) &       ! 4
            , vertex_t([1, 3], var_str("row_multiply")) &       ! 5
            , vertex_t([1, 4], var_str("row_subtract")) &       ! 6
            , vertex_t([1, 5], var_str("row_subtract")) &       ! 7
            , vertex_t([1, 6, 7], var_str("reconstruct")) &     ! 8
            , vertex_t([8], var_str("factor")) &                ! 9
            , vertex_t([8, 9], var_str("row_multiply")) &       ! 10
            , vertex_t([8, 10], var_str("row_subtract")) &      ! 11
            , vertex_t([8, 11], var_str("reconstruct")) &       ! 12
            , vertex_t([1], var_str("print")) &
            , vertex_t([8], var_str("print")) &
            , vertex_t([12], var_str("print")) &
            ])
        tasks = &
            [ task_item_t(initial_t(matrix)) &
            , task_item_t(calc_factor_t(row=2, step=1)) &
            , task_item_t(calc_factor_t(row=3, step=1)) &
            , task_item_t(row_multiply_t(step=1)) &
            , task_item_t(row_multiply_t(step=1)) &
            , task_item_t(row_subtract_t(row=2)) &
            , task_item_t(row_subtract_t(row=3)) &
            , task_item_t(reconstruct_t(step=1)) &
            , task_item_t(calc_factor_t(row=3, step=2)) &
            , task_item_t(row_multiply_t(step=2)) &
            , task_item_t(row_subtract_t(row=3)) &
            , task_item_t(reconstruct_t(step=2)) &
            , task_item_t(print_matrix_t()) &
            , task_item_t(print_matrix_t()) &
            , task_item_t(print_matrix_t()) &
            ]
        application = application_t(dag, tasks)
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

        real, allocatable :: matrix(:,:)
        real :: factor

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

        real, allocatable :: matrix(:,:)
        real :: factor
        real, allocatable :: new_row(:)
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

        real, allocatable :: matrix(:,:)
        real, allocatable :: row(:)
        real, allocatable :: new_row(:)
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

        real, allocatable :: original_matrix(:, :)
        real, allocatable :: new_matrix(:, :)
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

    function print_matrix_execute(self, arguments) result(output)
        class(print_matrix_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real, allocatable :: matrix(:,:)
        integer :: i

        associate(matrix_data => arguments(1)%raw_payload())
            associate(n_row => matrix_data(1), n_col => matrix_data(2))
                matrix = reshape(transfer(matrix_data(3:), matrix, n_row*n_col), [n_row, n_col])
                do i = 1, n_row
                    print *, matrix(i, :)
                end do
            end associate
        end associate
    end function
end module
