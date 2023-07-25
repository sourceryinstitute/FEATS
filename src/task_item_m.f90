module task_item_m
  !! define tasks for compute images to complete
  use payload_m, only: payload_t
  use task_m, only : task_t
  implicit none

  private
  public :: task_item_t

  type task_item_t
    !! A wrapper for a `class(task_t)` value, to facilitate constructing an array of tasks
    class(task_t), allocatable :: task
  contains
    procedure :: execute
  end type

contains
    function execute(self, arguments) result(output)
      !! complete the assigned task
      implicit none
      class(task_item_t), intent(in) :: self
      type(payload_t), intent(in)    :: arguments(:)
      type(payload_t) :: output

      output = self%task%execute(arguments)
    end function
end module task_item_m
