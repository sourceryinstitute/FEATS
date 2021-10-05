module mailbox_m
    use payload_m, only: payload_t
    implicit none
    private
    public :: payload_list_t, mailbox, mailbox_entry_can_be_freed

    type :: payload_list_t
        type(payload_t), allocatable :: payloads(:)
    end type

    type(payload_list_t), allocatable :: mailbox[:]
    !! storage for communicating inputs/outputs between tasks

    logical, allocatable :: mailbox_entry_can_be_freed(:)[:]
    !! used by the scheduler image to tell the worker images when they can release old data.
end module
