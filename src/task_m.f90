module task_m
  !! Define an abstract interface to tasks that the scheduler
  !! image assigns and that a compute image executes.
  use input_location_map_m, only: input_location_map_t
  use payload_item_m, only: payload_item_t
  implicit none

  private
  public :: task_t

  type, abstract :: task_t
    !! encapsulate task work
    private
  contains
    procedure(execute_i), deferred :: execute
  end type

  abstract interface

    subroutine execute_i(self, input_locations, mailbox)
      !! complete the assigned task
      import task_t
      implicit none
      class(task_t), intent(in) :: self
      type(input_location_map_t), intent(in) :: input_locations
      type(payload_item_t), intent(inout) :: mailbox(:)[:]
    end subroutine

  end interface

end module task_m
