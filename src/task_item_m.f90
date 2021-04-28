module task_item_m
  !! define tasks for compute images to complete
  use input_location_map_m, only: input_location_map_t
  use payload_item_m, only: payload_item_t
  use task_m, only : task_t
  implicit none

  private
  public :: task_item_t

  type task_item_t
    private
    class(task_t), allocatable :: task
  contains
    procedure :: execute
  end type

  interface task_item_t

    module function constructor(task) result(new_task_item)
      implicit none
      type(task_item_t) new_task_item
      class(task_t), intent(in) :: task
    end function

  end interface

  interface

    module subroutine execute(self, input_locations, mailbox)
      !! complete the assigned task
      implicit none
      class(task_item_t), intent(in) :: self
      type(input_location_map_t), intent(in) :: input_locations
      type(payload_item_t), intent(inout) :: mailbox(:)[:]
    end subroutine

  end interface

end module task_item_m
