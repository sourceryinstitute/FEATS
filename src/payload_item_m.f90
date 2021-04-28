module payload_item_m
    use payload_m, only: payload_t

    implicit none
    private
    public :: payload_item_t

    type :: payload_item_t
        private
        class(payload_t), allocatable :: payload_
    contains
        private
        procedure, public :: payload
    end type

    interface payload_item_t
        module function constructor(payload) result(payload_item)
            implicit none
            class(payload_t), intent(in) :: payload
            type(payload_item_t) :: payload_item
        end function
    end interface

    interface
        module function payload(self)
            implicit none
            class(payload_item_t), intent(in) :: self
            class(payload_t), allocatable :: payload
        end function
    end interface
end module
