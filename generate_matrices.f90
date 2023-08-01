program generate_matrices
    implicit none

    integer, parameter :: max_size = 50
    integer :: row, col, m_size, fu
    real, allocatable :: matrix(:, :)
    character(len=:), allocatable :: file_name
    character(len=2) :: size_string

    do m_size = 1, max_size
        allocate(matrix(m_size, m_size))
        do concurrent (row = 1:m_size, col = 1:m_size)
            matrix(row, col) = (row-1)*m_size + col
        end do
        write(size_string, "(I0)") m_size
        file_name = "example/lu_decomp/" // trim(size_string) // "x" // trim(size_string) // ".dat"
        open(newunit=fu, file=file_name, status="replace")
        do row = 1, m_size
            write(fu, *) matrix(row, :)
        end do
        close(fu)
        deallocate(matrix)
    end do
end program