module task_m
  !! define tasks for compute images to complete
  implicit none

  private
  public :: task_t

  integer, parameter :: no_work = 0

  type, abstract :: task_t
    !! encapsulate task identity and description
    private
    integer :: identifier_ = no_work
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
