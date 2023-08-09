program generate_matrices
    implicit none

    integer, parameter :: max_size = 100
    integer :: row, col, m_size, fu
    real, allocatable :: matrix(:, :)
    character(len=:), allocatable :: file_name
    character(len=3) :: size_string

    call random_init(.true., .true.)
    do m_size = 1, max_size
        allocate(matrix(m_size, m_size))
        call random_number(matrix)
        matrix = matrix*999 + 1 ! lets do numbers between 1 and 1000
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