module final_task_m
    use payload_m, only: payload_t
    use task_m, only: task_t

    implicit none
    private
    public :: final_task_t

    type, extends(task_t) :: final_task_t
        !! A signal to the compute images that all tasks have been completed,
        !! and thus they can stop waiting for tasks to be assigned.
    contains
        procedure :: execute
        procedure :: is_final_task
    end type
contains
    function execute(self, arguments) result(output)
        implicit none
        class(final_task_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output
    end function

    pure function is_final_task(self)
        implicit none
        class(final_task_t), intent(in) :: self
        logical :: is_final_task

        is_final_task = .true.
    end function
end module
