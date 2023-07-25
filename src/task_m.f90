module task_m
  !! Define an abstract interface to tasks that the scheduler
  !! image assigns and that a compute image executes.
  use payload_m, only: payload_t
  implicit none

  private
  public :: task_t

  type, abstract :: task_t
    !! encapsulate task work
  contains
    procedure(execute_i), deferred :: execute
  end type

  abstract interface

    function execute_i(self, arguments) result(output)
      !! complete the assigned task
      import :: task_t, payload_t
      implicit none
      class(task_t), intent(in)   :: self
      type(payload_t), intent(in) :: arguments(:)
      type(payload_t)             :: output
    end function

  end interface
end module task_m
