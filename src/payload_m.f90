module payload_m
    implicit none
    private
    public :: payload_t, empty_payload

    integer, parameter :: MAX_PAYLOAD_SIZE = 1000

    type :: payload_t
        !! A raw buffer to facilitate data transfer between  images
        !!
        !! Facilitates view of the data as either a string or raw bytes.
        !! Typical usage will be either to
        !! * produce a string representation of the data, and then parse that string to recover the original data
        !! * use the `transfer` function to copy the raw bytes of the data
        private
        integer, public :: payload_(MAX_PAYLOAD_SIZE)
        integer :: payload_size = 0
    contains
        private
        procedure, public :: raw_payload
        procedure, public :: string_payload
    end type

    interface payload_t
        pure module function from_raw(payload) result(new_payload)
            implicit none
            integer, intent(in) :: payload(:)
            type(payload_t) :: new_payload
        end function

        pure module function from_string(payload) result(new_payload)
            implicit none
            character(len=*), intent(in) :: payload
            type(payload_t) :: new_payload
        end function

        module procedure empty_payload
    end interface

    interface
        pure module function empty_payload()
            implicit none
            type(payload_t) :: empty_payload
        end function

        pure module function raw_payload(self)
            implicit none
            class(payload_t), intent(in) :: self
            integer, allocatable :: raw_payload(:)
        end function

        pure module function string_payload(self)
            implicit none
            class(payload_t), intent(in) :: self
            character(len=:), allocatable :: string_payload
        end function
    end interface

end module
