module runner_m
    !! Compute-image/Scheduler-image abstraction
    use dag_m, only: dag_t
    use iso_fortran_env, only: event_type, atomic_int_kind
    use payload_m, only: payload_t

    implicit none
    private
    public :: run
contains
    subroutine run(dag)
        type(dag_t), intent(in) :: dag

        integer(atomic_int_kind), parameter :: UNCLAIMED = 0_atomic_int_kind
        integer, parameter :: NONE_AVAILABLE = 0
        integer :: n_tasks, n_imgs, num_dependencies
        integer(atomic_int_kind) :: me
        integer(atomic_int_kind), allocatable :: taken_on(:)[:] !! Records who executed a given task
        integer(atomic_int_kind), allocatable :: num_tasks_completed[:] !! keep count of the tasks completed
        type(event_type), allocatable :: task_completed(:)[:] !! To notify the other images when task has been completed
        type(payload_t), allocatable :: mailbox(:)[:] !! storage for communicating inputs/outputs between tasks
        integer, allocatable :: dependencies(:)

        call initialize
        do while (not_all_tasks_completed())
            associate(next_task => find_next_task())
                if (next_task /= NONE_AVAILABLE) then
                    if (claim_task(next_task)) then
                        call execute_task(next_task)
                        call notify_task_completed(next_task)
                    end if
                end if
            end associate
        end do
    contains
        subroutine initialize()
            integer :: i
            n_tasks = size(dag%vertices)
            n_imgs = num_images()
            me = this_image()
            allocate( &
                taken_on(n_tasks)[*], &
                task_completed(n_tasks)[*], &
                mailbox(n_tasks)[*], &
                num_tasks_completed[*])
            if (me == 1) then
                do i = 1, n_tasks
                    call atomic_define(taken_on(i), UNCLAIMED)
                end do
                call atomic_define(num_tasks_completed, 0)
            end if
            sync all
        end subroutine

        function not_all_tasks_completed()
            logical :: not_all_tasks_completed

            integer :: tasks_completed

            call atomic_ref(tasks_completed, num_tasks_completed[1])
            not_all_tasks_completed = tasks_completed < n_tasks
        end function

        function find_next_task()
            integer :: find_next_task

            integer :: task, dependency, dependency_completed, taken_by

            task_loop: &
            do task = 1, n_tasks
                call atomic_ref(taken_by, taken_on(task)[1])
                if (taken_by == UNCLAIMED) then
                    dependencies = dag%dependencies_for(task)
                    num_dependencies = size(dependencies)
                    do dependency = 1, num_dependencies
                        call event_query(task_completed(dependencies(dependency)), dependency_completed)
                        if (dependency_completed == 0) cycle task_loop
                    end do
                    find_next_task = task
                    return
                end if
            end do task_loop
            find_next_task = NONE_AVAILABLE
        end function

        function claim_task(task)
            integer, intent(in) :: task
            logical :: claim_task

            integer(atomic_int_kind) :: old

            call atomic_cas(taken_on(task)[1], old, UNCLAIMED, me)
            claim_task = old == UNCLAIMED
        end function

        subroutine execute_task(task)
            integer, intent(in) :: task

            integer :: dependency
            type(payload_t), allocatable :: arguments(:)

            allocate(arguments(num_dependencies))
            do dependency = 1, num_dependencies
                event wait (task_completed(dependencies(dependency)))
            end do
            do concurrent (dependency = 1 : num_dependencies)
                arguments(dependency) = mailbox(dependencies(dependency))[taken_on(dependencies(dependency))[1]]
            end do
            mailbox(task) = dag%vertices(task)%task%execute(arguments)
        end subroutine

        subroutine notify_task_completed(task)
            integer, intent(in) :: task

            integer :: img, num_downstream, n

            num_downstream = size(dag%depends_on(task))
            do img = 1, n_imgs
                do n = 1, num_downstream
                    event post (task_completed(task)[img])
                end do
            end do
            call atomic_add(num_tasks_completed[1], 1)
        end subroutine
    end subroutine
end module
