module payload_m
    implicit none
    private
    public :: payload_t, empty_payload

    integer, parameter :: MAX_PAYLOAD_SIZE = 100

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
        module procedure from_raw
        module procedure from_string
        module procedure empty_payload
    end interface
contains
    pure function from_raw(payload) result(new_payload)
        implicit none
        integer, intent(in) :: payload(:)
        type(payload_t) :: new_payload

        integer :: incoming_payload_size

        incoming_payload_size = size(payload)
        if (incoming_payload_size > MAX_PAYLOAD_SIZE) then
            new_payload%payload_size = MAX_PAYLOAD_SIZE
            new_payload%payload_ = payload(1:MAX_PAYLOAD_SIZE)
        else
            new_payload%payload_size = incoming_payload_size
            new_payload%payload_(1:incoming_payload_size) = payload
        end if
    end function

    pure function from_string(payload) result(new_payload)
        implicit none
        character(len=*), intent(in) :: payload
        type(payload_t) :: new_payload

        new_payload%payload_(1) = len(payload)
        associate(string_as_integers => transfer(payload, new_payload%payload_))
            if (size(string_as_integers) > MAX_PAYLOAD_SIZE-1) then
                new_payload%payload_size = MAX_PAYLOAD_SIZE
                new_payload%payload_(2:MAX_PAYLOAD_SIZE) = string_as_integers(1:MAX_PAYLOAD_SIZE-1)
            else
                new_payload%payload_size = size(string_as_integers) + 1
                new_payload%payload_(2:new_payload%payload_size) = string_as_integers
            end if
        end associate
    end function

    pure function empty_payload()
        implicit none
        type(payload_t) :: empty_payload
    end function

    pure function raw_payload(self)
        implicit none
        class(payload_t), intent(in) :: self
        integer, allocatable :: raw_payload(:)

        raw_payload = self%payload_(1:self%payload_size)
    end function

    pure function string_payload(self)
        implicit none
        class(payload_t), intent(in) :: self
        character(len=:), allocatable :: string_payload

        if (self%payload_size > 0) then
            allocate(character(len=self%payload_(1)) :: string_payload)
            if (self%payload_(1) > 0) &
                    string_payload = transfer( &
                            self%payload_(2:self%payload_size), string_payload)
        else
            allocate(character(len=0) :: string_payload)
        end if
    end function
end module
