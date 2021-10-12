module payload_m
    use iso_fortran_env, only: int8

    implicit none
    private
    public :: payload_t, empty_payload

    type :: payload_t
        !! A raw buffer to facilitate data transfer between  images
        !!
        !! Facilitates view of the data as either a string or raw bytes.
        !! Typical usage will be either to
        !! * produce a string representation of the data, and then parse that string to recover the original data
        !! * use the `transfer` function to copy the raw bytes of the data
        private
        integer(int8), allocatable, public :: payload_(:)
    contains
        private
        procedure, public :: raw_payload
        procedure, public :: string_payload
    end type

    interface payload_t
        pure module function from_raw(payload) result(new_payload)
            implicit none
            integer(int8), intent(in) :: payload(:)
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
            integer(int8), allocatable :: raw_payload(:)
        end function

        pure module function string_payload(self)
            implicit none
            class(payload_t), intent(in) :: self
            character(len=:), allocatable :: string_payload
        end function
    end interface

end module
