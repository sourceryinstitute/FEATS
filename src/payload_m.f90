module payload_m
    implicit none
    private
    public :: payload_t

    type :: payload_t
        private
        character(len=:), allocatable :: payload_
    contains
        private
        procedure, public :: payload
    end type
    
    interface payload_t
        pure module function constructor(payload_) result(new_payload)
            implicit none
            character(len=*), intent(in) :: payload_
            type(payload_t) :: new_payload
        end function
    end interface
    
    interface
        pure module function payload(self)
            implicit none
            class(payload_t), intent(in) :: self
            character(len=:), allocatable :: payload
        end function
    end interface
end module
