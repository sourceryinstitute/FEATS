submodule(image_m) image_s
  use iso_fortran_env, only : event_type
  use assertions_interface, only : assert
  implicit none

  type(event_type), allocatable :: ready_for_next_task(:)[:]
  type(event_type) task_assigned[*]

  integer, parameter :: initial_team_scheduler = 1
  integer :: scheduler_image_ = initial_team_scheduler

contains

  module procedure set_up
    if (.not. allocated(ready_for_next_task)) allocate(ready_for_next_task(num_images())[*])
  end procedure

  module procedure assign_task
    event post(task_assigned[compute_image])
  end procedure

  module procedure wait_do_task_notify_ready
    if (.not. allocated(ready_for_next_task)) allocate(ready_for_next_task(num_images())[*])
    event wait(task_assigned)
    ! do task
    event post(ready_for_next_task(this_image())[scheduler_image_])
  end procedure

  module procedure is_scheduler
    image_is_scheduler = this_image() == scheduler_image_
  end procedure

  module procedure distribute_initial_tasks
    integer image

    do image=1, num_images()
      if (scheduler_image_ /= image) call self%assign_task(compute_image=image)
    end do
  end procedure

end submodule image_s
