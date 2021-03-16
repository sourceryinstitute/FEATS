submodule(compute_m) compute_s
  implicit none

contains

    module procedure is_this
      is_compute_image = image /= self%scheduler_image()
    end procedure

end submodule compute_s
