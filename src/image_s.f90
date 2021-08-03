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
    integer task_identifier[*]
contains
    module procedure run
        logical :: tasks_left

        tasks_left = .true.
        associate( &
                tasks => [application%tasks(), task_item_t(final_task_t())], &
                dag => application%dag())
            do while (tasks_left)
                if (this_image() == scheduler) then
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
                        mailbox)
                tasks_left = .true.
            else
                tasks_left = .false.
            end if
        end associate
    end function

    function assign_task(dag) result(tasks_left)
        type(dag_t), intent(in) :: dag
        logical :: tasks_left

        associate( &
                next_task => find_next_task(dag), &
                next_image => find_next_image())
            call assign_task_to_image(next_task, next_image)
        end associate
        if (.not.more_tasks(dag)) then
            call assign_completed_to_images()
            tasks_left = .false.
        else
            tasks_left = .true.
        end if
    end function
end submodule
