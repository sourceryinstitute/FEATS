module scheduler_m
  !! Compute-image/Scheduler-image abstraction
  use image_m, only : image_t
  use dag_interface, only : dag_t
  implicit none

  private
  public :: scheduler_t

  type, extends(image_t) :: scheduler_t
    !! Encapsulate scheduler image identity and communication protocol
    private
    type(dag_t) task_dependencies
  end type

end module
