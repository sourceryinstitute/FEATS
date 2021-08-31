module final_task_m
    use data_location_map_m, only: data_location_map_t
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

    interface

        module function execute(self, input_locations, task_number, mailbox) result(output)
            implicit none
            class(final_task_t), intent(in) :: self
            type(data_location_map_t), intent(in) :: input_locations
            integer, intent(in) :: task_number
            type(payload_t), intent(in) :: mailbox(:)[*]
            type(payload_t) :: output
        end function

        pure module function is_final_task(self)
            implicit none
            class(final_task_t), intent(in) :: self
            logical :: is_final_task
        end function

    end interface
end module
