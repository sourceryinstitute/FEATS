submodule(scheduler_m) scheduler_s
  implicit none

contains

  module procedure is_this_image
    scheduler_is_this_image = self%scheduler_image() == this_image()
  end procedure

end submodule scheduler_s
