module task_m
  !! Define an abstract interface to tasks that the scheduler
  !! image assigns and that a compute image executes.
  use payload_m, only: payload_t
  implicit none

  private
  public :: task_t

  type, abstract :: task_t
    !! encapsulate task work
    private
  contains
    procedure(execute_i), deferred :: execute
    procedure :: is_final_task
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
contains
    pure function is_final_task(self)
        !! is this the final task?
        implicit none
        class(task_t), intent(in) :: self
        logical :: is_final_task

        is_final_task = .false.
    end function
end module task_m
