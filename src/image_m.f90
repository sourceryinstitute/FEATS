module image_m
    !! Compute-image/Scheduler-image abstraction
    use application_m, only: application_t
    use dag_m, only: dag_t
    use final_task_m, only: final_task_t
    use iso_fortran_env, only: event_type
    use payload_m, only: payload_t
    use task_item_m, only: task_item_t

    implicit none
    private
    public :: image_t

    type image_t
        !! Encapsulate compute/scheduler communication protocol
        private
    contains
        private
        procedure, public :: run
    end type

    type :: payload_list_t
        type(payload_t), allocatable :: payloads(:)
    end type

    type(payload_list_t), allocatable :: mailbox[:]
    !! storage for communicating inputs/outputs between tasks

    logical, allocatable :: mailbox_entry_can_be_freed(:)[:]
    !! used by the scheduler image to tell the worker images when they can release old data.

    type(event_type), allocatable :: ready_for_next_task(:)[:]
    type(event_type) task_assigned[*]

    integer task_identifier[*]
    !! The ID of the task currently assigned to this image.

    integer, allocatable :: task_assignment_history(:)[:]
    !! Records which image did which task.
    !! Index: task number. Value: image number.

    logical, allocatable :: task_done(:)


    integer, parameter :: scheduler_image = 1
    integer, parameter :: no_task_assigned = -1
    integer, parameter :: NO_TASK_READY = -1
    integer, parameter :: ALL_TASKS_DONE = -2
    integer, parameter :: NO_IMAGE_READY = -1

contains

    subroutine run(self, application)
        implicit none
        class(image_t), intent(in) :: self
        type(application_t), intent(in) :: application

        logical :: tasks_left
        type(task_item_t), allocatable :: tasks(:)
        type(dag_t) :: dag

        task_identifier = no_task_assigned
        associate(n_tasks => size(application%tasks()), n_imgs => num_images())
            allocate(ready_for_next_task(n_imgs)[*])
            allocate(mailbox[*])
            allocate(mailbox%payloads(n_tasks))
            sync all
            allocate(mailbox_entry_can_be_freed(n_tasks)[*])
            mailbox_entry_can_be_freed(n_tasks) = .false.
            allocate(task_assignment_history(n_tasks)[*])
            task_assignment_history = NO_IMAGE_READY
            if (this_image() == scheduler_image) then
                allocate(task_done(n_tasks))
                task_done = .false.
            end if
        end associate

        tasks_left = .true.

        tasks = [application%tasks(), task_item_t(final_task_t())]
        dag = application%dag()
        do while (tasks_left)
            if (this_image() == scheduler_image) then
                tasks_left = assign_task(dag)
            else
                tasks_left = do_work(tasks, dag)
            end if
        end do
    end subroutine

    function do_work(tasks, dag) result(tasks_left)
        type(task_item_t), intent(in) :: tasks(:)
        type(dag_t),       intent(in) :: dag
        logical :: tasks_left

        event post(ready_for_next_task(this_image())[scheduler_image])
        event wait(task_assigned)

        !! It's probably better to introduce this only after some more testing -- HS
        !free_unneeded_memory: do concurrent(integer :: l = 0:size(mailbox))
        !    if(mailbox_entry_can_be_freed(i)) then
        !        deallocate(mailbox(i)%payload_)
        !        mailbox_entry_can_be_freed(i) = .false.
        !    end if
        !end do free_unneeded_memory

        do_assigned_task: associate(my_task    => tasks(task_identifier))
            if (.not.my_task%is_final_task()) then
                block
                    integer, allocatable :: upstream_task_nums(:)
                    integer, allocatable :: upstream_task_imagenums(:)
                    integer :: i
                    type(payload_t), allocatable :: arguments(:)

                    ! figure out which images have our input data
                    upstream_task_nums      = dag%dependencies_for(task_identifier)
                    upstream_task_imagenums = &
                        [(task_assignment_history(upstream_task_nums(i))[scheduler_image], i = 1, size(upstream_task_nums))]

                    arguments = [ ( payload_t(mailbox[upstream_task_imagenums(i)]%payloads(upstream_task_nums(i))%payload_), &
                                    i = 1, size(upstream_task_nums) ) ]

                    ! execute task, store result
                    mailbox%payloads(task_identifier) = &
                        my_task%execute(arguments)

                end block
                tasks_left = .true.
            else
                tasks_left = .false.
            end if
        end associate do_assigned_task
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
        integer :: i

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

                        ! check which task the image just finished, that's task A
                        ! for each task B upstream of A, walk through that task's downstream dependencies
                        ! if they're all completed, the output data from B can be freed.
                        i = task_identifier[next_image]
                        if (i /= NO_TASK_READY) then
                            upstream_tasks       = dag%dependencies_for(i)
                            upstream_task_images = task_assignment_history(upstream_tasks)
                            do i = 1, size(upstream_tasks)
                                if (all(task_done(dag%depends_on(upstream_tasks(i))))) then
                                    mailbox_entry_can_be_freed(upstream_tasks(i))[upstream_task_images(i)] = .true.
                                end if
                            end do
                        end if


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
!! find_next_task: search through the dag to find the next task where its
!! dependencies are complete
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
      integer, allocatable, dimension(:) :: dependencies
      logical :: done, all_done

      all_done = .true.
      next_task_to_run = NO_TASK_READY

      do task = 1, size(task_done)
         if ( task_done(task) .or. task_assignment_history(task) /= NO_IMAGE_READY ) then
            cycle
         else
            all_done = .false.
            dependencies = dag%dependencies_for ( task )
            done = .true.
            do depends = 1, size(dependencies)
               done = done .and. task_done(dependencies(depends))
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
end module
