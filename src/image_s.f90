submodule(image_m) image_s
    use dag_interface, only: dag_t
    use iso_fortran_env, only: event_type
    use final_task_m, only: final_task_t
    use mailbox_m, only: mailbox
    use task_item_m, only: task_item_t

    implicit none

    type(event_type), allocatable :: ready_for_next_task(:)[:]
    type(event_type) task_assigned[*]
    type(data_location_map_t), allocatable :: data_locations(:)[:]
    !! Contains the mappings of where inputs were computed for a given task.
    !!
    !! Will only contain information on the scheduler image.
    !! The map for a given task contains information on where it's inputs were computed.
    !! That map is present at the index corresponding to that task's ID.

    integer task_identifier[*]
    !! The ID of the task currently assigned to this image.

    integer, allocatable :: task_assignment_history(:)
    !! Records which image did which task.
    !! Index: task number. Value: image number.

    logical, allocatable :: task_done(:)


    integer, parameter :: scheduler_image = 1
    integer, parameter :: no_task_assigned = -1
    integer, parameter :: NO_TASK_READY = -1
    integer, parameter :: ALL_TASKS_DONE = -2
    integer, parameter :: NO_IMAGE_READY = -1

contains
    module procedure run
        logical :: tasks_left

        task_identifier = no_task_assigned
        associate(n_tasks => size(application%tasks()), n_imgs => num_images())
            allocate(ready_for_next_task(n_imgs)[*])
            allocate(data_locations(n_tasks)[*])
            allocate(mailbox(n_tasks)[*])
            if (this_image() == scheduler_image) then
                allocate(task_assignment_history(n_tasks))
                allocate(task_done(n_tasks))
                task_done = .false.
            end if
        end associate

        tasks_left = .true.

        associate( &
                tasks => [application%tasks(), task_item_t(final_task_t())], &
                dag => application%dag())
            do while (tasks_left)
                if (this_image() == scheduler_image) then
                    tasks_left = assign_task(dag)
                else
                    tasks_left = do_work(tasks)
                end if
            end do
        end associate
    end procedure

    function do_work(tasks) result(tasks_left)
        type(task_item_t), intent(in) :: tasks(:)
        logical :: tasks_left

        event post(ready_for_next_task(this_image())[scheduler_image])
        event wait(task_assigned)
        associate(my_task => tasks(task_identifier))
            if (.not.my_task%is_final_task()) then
                mailbox(task_identifier) = &
                    my_task%execute(data_locations(task_identifier)[scheduler_image], task_identifier, mailbox)
                tasks_left = .true.
            else
                tasks_left = .false.
            end if
        end associate
    end function

    function find_next_image() result(next_image)
        integer :: next_image, i, ev_count

        next_image = NO_IMAGE_READY
        do i = 1, size(ready_for_next_task)
            if (i == scheduler_image) cycle ! no need to check the scheduler image

            call event_query (ready_for_next_task(i), ev_count)
            if (ev_count > 0) then
                next_image = i
                associate (task_just_completed => (task_identifier[i]))
                    if (task_just_completed /= no_task_assigned) &
                        task_done(task_just_completed) = .true.
                end associate
            end if
        end do
    end function

    function assign_task(dag) result(tasks_left)
        type(dag_t), intent(in) :: dag
        logical :: tasks_left
        integer, allocatable, dimension(:) :: upstream_tasks, upstream_task_images

        associate(next_image => find_next_image())
            if (next_image /= NO_IMAGE_READY) then
                associate(next_task => find_next_task(dag))
                    if (next_task == NO_TASK_READY) then
                        tasks_left = .true.
                    else if (next_task == ALL_TASKS_DONE) then
                        call assign_completed_to_images()
                        tasks_left = .false.
                    else
                        event wait (ready_for_next_task(next_image))
                        task_assignment_history(next_task) = next_image

                        ! put together data location map
                        upstream_tasks       = dag%get_edges(next_task)
                        upstream_task_images = task_assignment_history(upstream_tasks)
                        data_locations(next_task) = data_location_map_t(upstream_tasks, upstream_task_images)

                        ! tell the image that it can proceed with the next task
                        task_identifier[next_image] = next_task
                        event post (task_assigned[next_image])
                        tasks_left = .true.
                    end if
                end associate
            else
                tasks_left = .true.
            end if
        end associate
    end function

    subroutine assign_completed_to_images()
        integer :: i

        do i = 1, size(ready_for_next_task)
            if (i == scheduler_image) cycle ! don't wait on the scheduler image

            event wait (ready_for_next_task(i))
            associate (task_just_completed => (task_identifier[i]))
                if (task_just_completed /= no_task_assigned) &
                    task_done(task_just_completed) = .true.
            end associate
            task_identifier[i] = size(task_done) + 1
            event post (task_assigned[i])
        end do
    end subroutine

    pure function find_next_task ( dag ) Result ( next_task_to_run )
!! find_next_task: search through the dag to find the next task where its dependents are complete
!!
!! possible outputs for next_task_to_run
!!     - a positive integer signals the next task to run
!!     - 'ALL_TASKS_DONE' signals all tasks are done
!!     - 'NO_TASK_READY' signals that no tasks are ready to run
!!
      implicit none

      type(dag_t), intent(in) :: dag
      integer :: next_task_to_run

      integer :: task, depends
      integer, allocatable, dimension(:) :: dependents
      logical :: done, all_done

      all_done = .true.
      next_task_to_run = NO_TASK_READY

      do task = 1, size(task_done)
         if ( task_done(task) ) then
            cycle
         else
            all_done = .false.
            dependents = dag%get_dependencies ( task )
            done = .true.
            do depends = 1, size(dependents)
               done = done .and. task_done(depends)
            end do
            if ( done ) then
               next_task_to_run = task
               exit
            else
               cycle
            end if
         end if
      end do


      if ( all_done ) then
         next_task_to_run = ALL_TASKS_DONE
      end if

    end function find_next_task
end submodule
