submodule(scheduler_m) scheduler_s
  use compute_m, only : compute_t
  implicit none

contains

  module procedure is_this_image
    scheduler_is_this_image = self%scheduler_image() == this_image()
  end procedure

  module procedure distribute_initial_tasks
    type(compute_t) compute
    integer image

    if (self%is_this_image()) then
      do image=1,num_images()
        if (compute%is_this(image)) call self%assign_task(compute_image=image)
      end do
    else
      call compute%wait_do_task_notify_ready
    end if
  end procedure

end submodule scheduler_s
