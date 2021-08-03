module task_m
  !! Define an abstract interface to tasks that the scheduler
  !! image assigns and that a compute image executes.
  use data_location_map_m, only: data_location_map_t
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

    subroutine execute_i(self, input_locations, mailbox)
      !! complete the assigned task
      import :: data_location_map_t, task_t, payload_t
      implicit none
      class(task_t), intent(in) :: self
      type(data_location_map_t), intent(in) :: input_locations
      type(payload_t), intent(inout) :: mailbox(:)[*]
    end subroutine

  end interface

  interface

    pure function is_final_task(self)
        !! is this the final task?
        import :: task_t
        implicit none
        class(task_t), intent(in) :: self
        logical :: is_final_task_i
    end function

  end interface

end module task_m
