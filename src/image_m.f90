module image_m
  !! Compute-image/Scheduler-image abstraction
  use task_m, only : task_t
  implicit none

  private
  public :: image_t

  type image_t
    !! Encapsulate compute/scheduler communication protocol
    private
  contains
    procedure, nopass :: distribute_and_do_initial_tasks
  end type

  interface

    module subroutine distribute_and_do_initial_tasks(tasks)
      !! Scheduler places tasks in each compute image's mailbox.
      !! Compute-image does task.
      implicit none
      type(task_t), intent(in) :: tasks(:)
    end subroutine

  end interface

end module
