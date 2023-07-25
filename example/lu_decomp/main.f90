program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use lu_decomp_app_m, only : generate_application
  use runner_m, only: run
  implicit none

  call run(generate_application())
end program
