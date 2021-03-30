module task_item_m
  !! define tasks for compute images to complete
  use task_m, only : task_t
  implicit none

  private
  public :: task_item_t

  type task_item_t
    private
    class(task_t), allocatable :: task
  contains
    procedure :: do_work
  end type

  interface task_item_t

    module function constructor(task) result(new_task_item)
      implicit none
      type(task_item_t) new_task_item
      class(task_t), intent(in) :: task
    end function

  end interface

  interface

    module subroutine do_work(self)
      !! complete the assigned task
      implicit none
      class(task_item_t), intent(in) :: self
    end subroutine

  end interface

end module task_item_m
