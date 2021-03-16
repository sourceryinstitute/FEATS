module image_m
  !! Compute-image/Scheduler-image abstraction
  use iso_fortran_env, only : event_type
  implicit none

  private
  public :: image_t

  type image_t
    !! Encapsulate compute/scheduler communication protocol
    private

    integer :: scheduler_image_ = 1
  contains
    procedure :: set_up
    procedure :: assign_task
    procedure :: scheduler_image
    procedure :: wait_do_task_notify_ready
  end type

  interface

    module subroutine set_up(self)
      !! Synchronized allocation of the event_type inbox coarray
      implicit none
      class(image_t), intent(out) :: self
    end subroutine

    module subroutine assign_task(self, compute_image)
      !! Post a new task to the designated compute image
      implicit none
      class(image_t), intent(in) :: self
      integer, intent(in) :: compute_image
    end subroutine

    module subroutine wait_do_task_notify_ready(self)
      !! Compute compute does task; error terminate if called on scheduler compute
      implicit none
      class(image_t), intent(in) :: self
    end subroutine

    module function scheduler_image(self) result(image_number)
      !! Get scheduler image number
      implicit none
      class(image_t), intent(in) :: self
      integer image_number
    end function

  end interface

end module
