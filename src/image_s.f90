submodule(image_m) image_s
  use iso_fortran_env, only : event_type
  use assertions_interface, only : assert, max_errmsg_len
  implicit none

  type(event_type), allocatable :: ready_for_next_task(:)[:]
  type(event_type) task_assigned[*]

  integer, parameter :: initial_team_scheduler = 1, success=0
  integer :: scheduler_image = initial_team_scheduler

contains

  subroutine wait_do_task_notify_ready
    !! Compute-image does task
    character(len=max_errmsg_len) :: message
    integer status

    if (.not. allocated(ready_for_next_task)) allocate(ready_for_next_task(num_images())[*], stat=status, errmsg=message)
    call assert(status == success, "image_t%distribute_initial_tasks: stat == status")

    event wait(task_assigned)
    ! do task
    event post(ready_for_next_task(this_image())[scheduler_image])
  end subroutine

  pure function is_scheduler()  result(image_is_scheduler)
    !! Result is .true. iff the executing image is the scheduler
    logical image_is_scheduler
    image_is_scheduler = this_image() == scheduler_image
  end function

   subroutine distribute_initial_tasks
    !! Scheduler places tasks in each compute image's mailbox
    character(len=max_errmsg_len) :: message
    integer image, status

    if (.not. allocated(ready_for_next_task)) allocate(ready_for_next_task(num_images())[*], stat=status, errmsg=message)
    call assert(status == success, "image_t%distribute_initial_tasks: stat == status")

    do image=1, num_images()
      if (scheduler_image /= image) event post(task_assigned[image])
    end do
  end subroutine

  module procedure distribute_and_do_initial_tasks
    if (is_scheduler()) then
      call distribute_initial_tasks
    else
      call wait_do_task_notify_ready
    end if
  end procedure

end submodule image_s
