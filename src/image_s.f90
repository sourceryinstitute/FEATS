submodule(image_m) image_s
  use assertions_interface, only : assert
  implicit none

  type(event_type), allocatable :: ready_for_next_task_(:)[:]
  type(event_type) task_assigned_[*]

contains

  module procedure set_up
    logical, save :: first_call = .true.

    call assert(first_call, "image_t%set_up: first_call")

    first_call = .false.
    allocate(ready_for_next_task_(num_images())[*])

  end procedure

  module procedure assign_task
    event post(task_assigned_[compute_image])
  end procedure

  module procedure wait_do_task_notify_ready
    call assert(allocated(ready_for_next_task_), "image_t%wait_do_task_notify_ready: allocated(self%ready_for_next_task_)")

    event wait(task_assigned_)
    ! do task
    event post(ready_for_next_task_(this_image())[self%scheduler_image_])
  end procedure

  module procedure scheduler_image
    image_number = self%scheduler_image_
  end procedure

end submodule image_s
