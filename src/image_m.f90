module image_m
  !! Compute-image/Scheduler-image abstraction
  implicit none

  private
  public :: image_t

  type image_t
    !! Encapsulate compute/scheduler image identity and communication protocol
    private
    logical :: scheduler_ = .false.
  contains
    procedure :: set_up
    procedure :: scheduler
    procedure :: compute_tells_scheduler_im_ready
    procedure :: scheduler_assigns_task
  end type

  interface

    module subroutine set_up(self)
      !! Self-identify as a scheduler image or a compute image and synchronized
      !! allocation of the scheduler image's event inbox array; synchronize;
      !! error terminate if called more than once.
      implicit none
      class(image_t), intent(out) :: self
    end subroutine

    pure module function scheduler(self) result(I_am_scheduler)
      !! Result is .true. if the executing image is the scheduler image, .false. otherwise
      implicit none
      class(image_t), intent(in) :: self
      logical I_am_scheduler
    end function

    module subroutine compute_tells_scheduler_im_ready(self)
      !! Compute image notifies scheduler image that compute image is ready to do a new task;
      !! error terminate if called on scheduler image or if task-readiness list is unallocated on scheduler
      implicit none
      class(image_t), intent(in) :: self
    end subroutine

    module subroutine scheduler_assigns_task(self, compute_image)
      !! Scheduler image posts a new task to the designated compute image;
      !! error terminate if called on compute image
      implicit none
      class(image_t), intent(in) :: self
      integer, intent(in) :: compute_image
    end subroutine

    module subroutine do_task(self)
      !! Compute image does task; error terminate if called on scheduler image
      implicit none
      class(image_t), intent(in) :: self
    end subroutine

  end interface

end module
