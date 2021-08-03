module payload_m
    implicit none
    private
    public :: payload_t

    type :: payload_t
        private
        character(len=1), allocatable :: payload_(:)
    contains
        private
        procedure, public :: raw_payload
        procedure, public :: string_payload
    end type
    
    interface payload_t
        pure module function from_raw(payload) result(new_payload)
            implicit none
            character(len=1), intent(in) :: payload(:)
            type(payload_t) :: new_payload
        end function

        pure module function from_string(payload) result(new_payload)
            implicit none
            character(len=*), intent(in) :: payload
            type(payload_t) :: new_payload
        end function
    end interface
    
    interface
        pure module function raw_payload(self)
            implicit none
            class(payload_t), intent(in) :: self
            character(len=1), allocatable :: raw_payload(:)
        end function

        pure module function string_payload(self)
            implicit none
            class(payload_t), intent(in) :: self
            character(len=:), allocatable :: string_payload
        end function
    end interface
end module
