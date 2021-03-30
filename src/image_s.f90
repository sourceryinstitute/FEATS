submodule(image_m) image_s
  use iso_fortran_env, only : event_type
  use assertions_interface, only : assert, max_errmsg_len
  implicit none

  type(event_type), allocatable :: ready_for_next_task(:)[:]
  type(event_type) task_assigned[*]
  type(task_t) task[*]

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
    ! call task%do()
    event post(ready_for_next_task(this_image())[scheduler_image])
  end subroutine

   subroutine distribute_initial_tasks(tasks)
    !! Scheduler places tasks in each compute image's mailbox
    type(task_t), intent(in) :: tasks(:)
    character(len=max_errmsg_len) :: message
    integer image, status, i

    if (.not. allocated(ready_for_next_task)) allocate(ready_for_next_task(num_images())[*], stat=status, errmsg=message)
    call assert(status == success, "image_t%distribute_initial_tasks: stat == status")

    i = 0
    do image=1, min(num_images(), size(tasks))
      if (scheduler_image /= image) then
        i = i + 1
        task[image] = tasks(i)
        event post(task_assigned[image])
      end if
    end do
  end subroutine

  module procedure distribute_and_do_initial_tasks
    if (this_image() == scheduler_image) then
      call distribute_initial_tasks(tasks)
    else
      call wait_do_task_notify_ready
    end if
  end procedure

end submodule image_s
