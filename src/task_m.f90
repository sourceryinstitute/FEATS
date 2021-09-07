module task_m
  !! Define an abstract interface to tasks that the scheduler
  !! image assigns and that a compute image executes.
  use payload_m, only: payload_t, task_result_t
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

    function execute_i(self, task_number, upstream_task_results) result(output)
      !! complete the assigned task
      import :: task_t, payload_t, task_result_t
      implicit none
      class(task_t), intent(in) :: self
      integer, intent(in) :: task_number
      class(task_result_t), intent(in) :: upstream_task_results(:)
      type(payload_t) :: output
    end function

  end interface

  interface

    pure module function is_final_task(self)
        !! is this the final task?
        implicit none
        class(task_t), intent(in) :: self
        logical :: is_final_task
    end function

  end interface

end module task_m
