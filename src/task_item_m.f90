module task_item_m
  !! define tasks for compute images to complete
  use payload_m, only: payload_t
  use task_m, only : task_t
  implicit none

  private
  public :: task_item_t

  type task_item_t
    !! A wrapper for a `class(task_t)` value, to facilitate constructing an array of tasks
    private
    class(task_t), allocatable :: task
  contains
    procedure :: execute
    procedure :: is_final_task
  end type

  interface task_item_t
    module procedure constructor
  end interface
contains
    function constructor(task) result(new_task_item)
      implicit none
      type(task_item_t) new_task_item
      class(task_t), intent(in) :: task

      new_task_item%task = task
    end function

    function execute(self, arguments) result(output)
      !! complete the assigned task
      implicit none
      class(task_item_t), intent(in) :: self
      type(payload_t), intent(in)    :: arguments(:)
      type(payload_t) :: output

      output = self%task%execute(arguments)
    end function

    pure function is_final_task(self)
        !! is this the final task?
        implicit none
        class(task_item_t), intent(in) :: self
        logical :: is_final_task

        is_final_task = self%task%is_final_task()
    end function
end module task_item_m
