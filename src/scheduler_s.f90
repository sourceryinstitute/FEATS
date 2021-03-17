submodule(scheduler_m) scheduler_s
  use compute_m, only : compute_t
  implicit none

contains

  module procedure is_this_image
    scheduler_is_this_image = self%scheduler_image() == this_image()
  end procedure

  module procedure distribute_initial_tasks
    integer image

    associate(compute => compute_t())
      do image=1, num_images()
        if (compute%is_this(image)) call self%assign_task(compute_image=image)
      end do
    end associate
  end procedure

end submodule scheduler_s
