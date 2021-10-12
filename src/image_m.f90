module image_m
  !! Compute-image/Scheduler-image abstraction
  use application_m, only: application_t
  use payload_m, only: payload_t
  use feats_result_map_m, only: feats_result_map_t
  implicit none

  private
  public :: image_t

  type image_t
    !! Encapsulate compute/scheduler communication protocol
    private
  contains
    private
    procedure, public :: run
  end type

  interface

    module function run(self, application) result(results)
      implicit none
      class(image_t), intent(in) :: self
      type(application_t), intent(in) :: application
      type(feats_result_map_t), allocatable :: results
    end function

  end interface

end module
