module task_m
  !! define tasks for compute images to complete
  implicit none

  private

  integer, parameter :: no_work = 0

  type, public :: task_t
    !! encapsulate task identity and description
    private
    integer :: identifier_ = no_work
  contains
    procedure :: do_work
  end type

  interface

    module subroutine do_work(this, verbose)
      !! complete the assigned task
      implicit none
      class(task_t), intent(in) :: this
      logical, intent(in), optional :: verbose
    end subroutine

  end interface

end module task_m
