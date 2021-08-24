submodule(image_m) image_s
    use dag_interface, only: dag_t
    use data_location_map_m, only: data_location_map_t
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

contains
    module procedure run
        logical :: tasks_left

        task_identifier = no_task_assigned
        associate(n_tasks => size(application%tasks()), n_imgs => num_images())
            allocate(ready_for_next_task(n_imgs))
            allocate(data_locations(n_tasks))
            allocate(mailbox(n_tasks))
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

    function do_work(tasks_left, tasks) result(tasks_left)
        type(task_item_t), intent(in) :: tasks(:)
        logical :: tasks_left

        event post(ready_for_next_task(this_image())[scheduler_image])
        event wait(task_assigned)
        associate(my_task => tasks(task_identifier))
            if (.not.my_task%is_final_task()) then
                call my_task%execute( &
                        data_locations(task_identifier)[scheduler_image], &
                        task_identifier, &
                        mailbox)
                tasks_left = .true.
            else
                tasks_left = .false.
            end if
        end associate
    end function

    function find_next_image() result(next_image)
        integer :: next_image, i, ev_count

        do i = 1, size(ready_for_next_task)
            if (i == scheduler_image) cycle ! no need to check the scheduler image

            call event_query (ready_for_next_task(i), ev_count)
            if (ev_count > 0) then
                event wait (ready_for_next_task(i))
                next_image = i
                associate (task_just_completed => task_identifier[i])
                    if (task_just_completed /= no_task_assigned) &
                        task_done(task_just_completed) = .true.
                end associate
                exit
            end if
        end do
    end function

    function assign_task(dag) result(tasks_left)
        type(dag_t), intent(in) :: dag
        logical :: tasks_left

        associate( &
                next_task => find_next_task(dag), &
                next_image => find_next_image())
            ! track where we're assigning this task to
            task_assignment_history(next_task) = next_image
            ! put together data location map.
            !data_locations(next_task) = data_location_map_t()
            ! tell the image that it can proceed with the next task
            task_identifier[next_image] = next_task
            event post (task_assigned[next_image])
        end associate
        if (.not.more_tasks(dag)) then
            call assign_completed_to_images()
            tasks_left = .false.
        else
            tasks_left = .true.
        end if
    end function
end submodule
