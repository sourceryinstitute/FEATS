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
  contains
    procedure :: is_this_image
  end type

  interface

    module function is_this_image(self) result(scheduler_is_this_image)
      !! Result is .true. if the executing image is the scheduler image
      implicit none
      class(scheduler_t), intent(in) :: self
      logical scheduler_is_this_image
    end function

  end interface

end module
