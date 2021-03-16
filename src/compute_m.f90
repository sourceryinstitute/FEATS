module compute_m
  !! Compute-image class
  use image_m, only : image_t
  implicit none

  private
  public :: compute_t

  type, extends(image_t) :: compute_t
    !! Encapsulate compute-image communication protocol
    private
  contains
    procedure :: is_this
  end type

  interface

    module function is_this(self, image) result(is_compute_image)
      !! Result is .true. if the executive image is a compute image
      implicit none
      class(compute_t), intent(in) :: self
      integer, intent(in) :: image
      logical is_compute_image
    end function

  end interface


end module compute_m
