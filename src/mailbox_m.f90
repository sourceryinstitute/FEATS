module mailbox_m
    use payload_m, only: payload_t
    implicit none
    private
    public :: mailbox, mailbox_entry_can_be_freed

    type(payload_t), allocatable :: mailbox(:)[:]
    !! storage for communicating inputs/outputs between tasks
    !!
    !! tasks should not access this directly, as it is passed as an argument to `execute`

    logical, allocatable :: mailbox_entry_can_be_freed(:)[:]
    !! used by the scheduler image to tell the worker images when they can release old data. 
end module
