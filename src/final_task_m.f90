module final_task_m
    use data_location_map_m, only: data_location_map_t
    use payload_m, only: payload_t
    use task_m, only: task_t

    implicit none
    private
    public :: final_task_t

    type, extends(task_t) :: final_task_t
    contains
        procedure :: execute
        procedure :: is_final_task
    end type

    interface

        module subroutine execute(self, input_locations, mailbox)
            implicit none
            class(final_task_t), intent(in) :: self
            type(data_location_map_t), intent(in) :: input_locations
            type(payload_t), intent(inout) :: mailbox(:)[*]
        end subroutine

        pure module function is_final_task(self)
            implicit none
            class(final_task_t), intent(in) :: self
            logical :: is_final_task
        end function

    end interface
end module