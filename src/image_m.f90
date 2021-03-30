module image_m
  !! Compute-image/Scheduler-image abstraction
  implicit none

  private
  public :: image_t

  type image_t
    !! Encapsulate compute/scheduler communication protocol
    private
  contains
    procedure :: distribute_and_do_initial_tasks
    procedure, nopass, private :: wait_do_task_notify_ready
    procedure, nopass, private :: is_scheduler
    procedure, nopass, private :: distribute_initial_tasks
  end type

  interface

    module subroutine distribute_and_do_initial_tasks(self)
      !! Scheduler places tasks in each compute image's mailbox.
      !! Compute-image does task.
      implicit none
      class(image_t), intent(in) :: self
    end subroutine

    module subroutine wait_do_task_notify_ready()
      !! Compute-image does task
      implicit none
    end subroutine

    pure module function is_scheduler() result(image_is_scheduler)
      !! Result is .true. iff the executing image is the scheduler
      implicit none
      logical image_is_scheduler
    end function

    module subroutine distribute_initial_tasks()
      !! Scheduler places tasks in each compute image's mailbox
      implicit none
    end subroutine

  end interface

end module
