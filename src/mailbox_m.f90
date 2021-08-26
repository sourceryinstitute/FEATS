module mailbox_m
    use payload_m, only: payload_t
    implicit none
    private
    public :: mailbox

    type(payload_t), allocatable :: mailbox(:)[:]
    !! storage for communicating inputs/outputs between tasks
    !!
    !! tasks should not access this directly, as it is passed as an argument to `execute`
end module
