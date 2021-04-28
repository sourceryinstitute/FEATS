module image_m
  !! Compute-image/Scheduler-image abstraction
  use application_m, only: application_t
  use data_location_map_m, only: data_location_map_t
  implicit none

  private
  public :: image_t

  type image_t
    !! Encapsulate compute/scheduler communication protocol
    private
  contains
    private
    procedure :: run
  end type

  interface

    module function run(self, application) result(results_locations)
      implicit none
      class(image_t), intent(in) :: self
      type(application_t), intent(in) :: application
      type(data_location_map_t), intent(inout) :: results_locations
    end function

  end interface

end module
