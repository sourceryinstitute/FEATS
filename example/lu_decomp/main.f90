program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use dag_m, only: dag_t
  use iso_fortran_env, only: int64
  use lu_decomp_app_m, only : generate_application
  use runner_m, only: run
  implicit none

  character(len=*), parameter :: timing_file = "timings.csv"
  type(dag_t) :: tasks
  integer :: matrix_size
  integer(int64) :: start, finish, count_rate
  logical :: file_exists
  integer :: f_unit

  if (this_image() == 1) call system_clock(start, count_rate)
  call generate_application(tasks, matrix_size)
  call run(tasks)
  if (this_image() == 1) call system_clock(finish)

  if (this_image() == 1) then
    inquire(file=timing_file, exist=file_exists)
    if (.not.file_exists) then
      open(file=timing_file, newunit = f_unit, status="new")
      write(f_unit, '(A)') "matrix size,num tasks,num images,execution time"
      close(f_unit)
    end if
    open(file=timing_file, newunit = f_unit, status="old", position = "append")
    write(f_unit, '(I0, ",", I0, ",", I0, ",", G0)') matrix_size, size(tasks%vertices), num_images(), real(finish-start)/real(count_rate)
  end if
end program
