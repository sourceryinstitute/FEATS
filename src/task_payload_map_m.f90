module task_payload_map_m
    use payload_m, only: payload_t
    implicit none
    private
    public :: task_payload_map_t

    type :: task_payload_map_t
        !! Mapping from a task ID -> the payload that task output
        private
        integer, allocatable         :: task_ids_(:)
        type(payload_t), allocatable :: payloads_(:)
    contains
        private
        procedure, public :: get_task_ids
        procedure, public :: get_payload
    end type

    interface task_payload_map_t
        pure module function constructor(task_ids, payloads) result(new_map)
            implicit none
            integer, intent(in)          :: task_ids(:)
            class(payload_t), intent(in) :: payloads(:)
            type(task_payload_map_t) :: new_map
        end function
    end interface

    interface
        pure module function get_task_ids(self) result(ids)
            implicit none
            class(task_payload_map_t), intent(in) :: self
            integer, allocatable :: ids(:)
        end function

        pure module function get_payload(self, taskid) result(p)
            implicit none
            class(task_payload_map_t), intent(in) :: self
            integer, intent(in) :: taskid
            type(payload_t) :: p
        end function
    end interface
end module
