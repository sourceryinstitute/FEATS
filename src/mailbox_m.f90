module mailbox_m
    use payload_m, only: payload_t
    implicit none
    private
    public :: mailbox

    type(payload_t), allocatable :: mailbox(:)[:]
end module
