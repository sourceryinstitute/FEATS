module payload_m
    implicit none
    private
    public :: payload_t, maybe_payload_t

    type :: payload_t
        !! A raw buffer to facilitate data transfer between  images
        !!
        !! Facilitates view of the data as either a string or raw bytes.
        !! Typical usage will be either to
        !! * produce a string representation of the data, and then parse that string to recover the original data
        !! * use the `transfer` function to copy the raw bytes of the data
        private
        character(len=1), allocatable, public :: payload_(:)
    contains
        private
        procedure, public :: raw_payload
        procedure, public :: string_payload
    end type

    type :: maybe_payload_t
        !! A type that can represent a payload or an error condition
        private
        logical :: has_payload_
        type(payload_t) :: payload_
    contains
        private
        procedure, public :: check
        procedure, public :: get
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

    interface maybe_payload_t
        pure module function no_payload() result(m)
            implicit none 
            type(maybe_payload_t) :: m
        end function

        pure module function some_payload(p) result(m)
            implicit none
            type(maybe_payload_t) :: m
            class(payload_t), intent(in) :: p
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

        pure module function check(self)
            implicit none
            class(maybe_payload_t), intent(in) :: self
            logical :: check
        end function 

        pure module function get(self)
            implicit none
            class(maybe_payload_t), intent(in) :: self
            type(payload_t) :: get
        end function
    end interface

end module
