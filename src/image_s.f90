submodule(image_m) image_s
  use iso_fortran_env, only : event_type
  use assertions_interface, only : assert
  implicit none

  type(event_type), allocatable :: ready_for_next_task(:)[:]
  type(event_type) task_assigned[*]

  integer, parameter :: scheduler_image = 1

contains

  module procedure set_up

    logical, save :: first_call = .true.

    call assert(first_call, "image_t%set_up: first_call")

    first_call = .false.

    if (this_image() == scheduler_image) self%scheduler_ = .true.
    allocate(ready_for_next_task(2:num_images())[*])

  end procedure

  module procedure scheduler
    I_am_scheduler = this_image() == scheduler_image
  end procedure

  module procedure scheduler_assigns_task

    call assert(self%scheduler(), "image_t%scheduler_assigns_task: self%scheduler()")

    event post(task_assigned[compute_image])

  end procedure

  module procedure wait_do_task_notify_ready

    call assert(.not. self%scheduler(), "image_t%wait_do_task_notify_ready: .not. self%scheduler()")
    call assert(allocated(ready_for_next_task), "image_t%wait_do_task_notify_ready: allocated(ready_for_next_task)")

    event wait(task_assigned)

    ! do task

    event post(ready_for_next_task(this_image())[scheduler_image])

  end procedure

end submodule image_s
