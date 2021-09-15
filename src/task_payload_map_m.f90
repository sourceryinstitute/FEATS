module task_payload_map_m
    use payload_m, only: payload_t
    implicit none
    private
    public :: task_payload_map_t

    type :: task_payload_map_t
        !! Mapping from a task ID -> the payload that task output
        private
        integer, allocatable :: task_ids_(:)
        integer, allocatable :: image_nums_(:)
    contains
        private
        procedure, public :: get_task_ids
        procedure, public :: get_raw_payload, get_string_payload
    end type

    interface task_payload_map_t
        pure module function constructor(task_ids, image_nums) result(new_map)
            implicit none
            integer, intent(in) :: task_ids(:)
            integer, intent(in) :: image_nums(:)
            type(task_payload_map_t) :: new_map
        end function
    end interface

    interface
        pure module function get_task_ids(self) result(ids)
            implicit none
            class(task_payload_map_t), intent(in) :: self
            integer, allocatable :: ids(:)
        end function

        pure module subroutine get_raw_payload(self, taskid, pl, key_error) 
            implicit none
            class(task_payload_map_t), intent(in) :: self
            integer, intent(in) :: taskid
            character(len=1), allocatable, intent(out) :: pl(:)
            logical, optional, intent(out) :: key_error
        end subroutine

        pure module subroutine get_string_payload(self, taskid, pl, key_error) 
            implicit none
            class(task_payload_map_t), intent(in) :: self
            integer, intent(in) :: taskid
            character(len=:), allocatable, intent(out) :: pl
            logical, optional, intent(out) :: key_error
        end subroutine

    end interface
end module
