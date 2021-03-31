module task_m
  !! Define an abstract interface to tasks that the scheduler
  !! image assigns and that a compute image executes.
  implicit none

  private
  public :: task_t

  integer, parameter :: no_work = 0

  type, abstract :: task_t
    !! encapsulate task work
    private
  contains
    procedure(do_work_interface), deferred :: do_work
  end type

  abstract interface

    subroutine do_work_interface(self)
      !! complete the assigned task
      import task_t
      implicit none
      class(task_t), intent(in) :: self
    end subroutine

  end interface

end module task_m
