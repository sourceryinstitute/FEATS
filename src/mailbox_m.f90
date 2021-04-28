module mailbox_m
    use payload_item_m, only: payload_item_t
    implicit none
    private
    public :: mailbox

    type(payload_item_t), allocatable :: mailbox(:)[:]
end module
