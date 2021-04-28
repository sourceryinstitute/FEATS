module image_m
  !! Compute-image/Scheduler-image abstraction
  use application_m, only: application_t
  use payload_item_m, only: payload_item_t
  implicit none

  private
  public :: image_t

  type image_t
    !! Encapsulate compute/scheduler communication protocol
    private
    type(payload_item_t), allocatable :: mailbox(:)[:]
  contains
    private
    procedure :: run
  end type

  interface

    module subroutine run(self, application, results_data)
      implicit none
      class(image_t), intent(in) :: self
      type(application_t), intent(in) :: application
      type(results_data_t), intent(inout) :: results_data_t
  end subroutine

  end interface

end module
