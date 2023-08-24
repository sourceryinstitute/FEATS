module payload_m
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
        integer, public, allocatable :: payload_(:)
    contains
        private
        procedure, public :: raw_payload
        procedure, public :: string_payload
    end type

    interface payload_t
        module procedure from_raw
        module procedure from_string
        module procedure empty_payload
    end interface
contains
    pure function from_raw(payload) result(new_payload)
        implicit none
        integer, intent(in) :: payload(:)
        type(payload_t) :: new_payload

        allocate(new_payload%payload_, source = payload)
    end function

    pure function from_string(payload) result(new_payload)
        implicit none
        character(len=*), intent(in) :: payload
        type(payload_t) :: new_payload

        new_payload = payload_t([len(payload), transfer(payload, new_payload%payload_)])
    end function

    pure function empty_payload()
        implicit none
        type(payload_t) :: empty_payload

        allocate(empty_payload%payload_(0))
    end function

    pure function raw_payload(self)
        implicit none
        class(payload_t), intent(in) :: self
        integer, allocatable :: raw_payload(:)

        raw_payload = self%payload_
    end function

    pure function string_payload(self)
        implicit none
        class(payload_t), intent(in) :: self
        character(len=:), allocatable :: string_payload

        if (size(self%payload_) > 0) then
            allocate(character(len=self%payload_(1)) :: string_payload)
            if (self%payload_(1) > 0) &
                    string_payload = transfer( &
                            self%payload_(2:), string_payload)
        else
            allocate(character(len=0) :: string_payload)
        end if
    end function
end module
