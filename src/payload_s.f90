submodule(payload_m) payload_s
    implicit none
contains
    module procedure from_raw
        new_payload%payload_ = payload
    end procedure

    module procedure from_string
        integer :: i

        associate(length => len(payload))
            allocate(new_payload%payload_(length))
            do concurrent (i = 1 : length)
                new_payload%payload_(i) = payload(i:i)
            end do
        end associate
    end procedure

    module procedure raw_payload
        if (allocated(self%payload_)) then
            raw_payload = self%payload_
        else
            raw_payload = [character(len=1)::]
        end if
    end procedure

    module procedure string_payload
        integer :: i

        associate(length => size(self%payload_))
            allocate(character(len=length) :: string_payload)
            do concurrent (i = 1 : length)
                string_payload(i:i) = self%payload_(i)
            end do
        end associate
    end procedure

    module procedure no_payload
        m%has_payload_ = .false.
    end procedure

    module procedure some_payload
        m%has_payload_ = .true.
        m%payload_ = p
    end procedure

    module procedure check
        check = self%has_payload_
    end procedure

    module procedure get
        if(.not. self%check()) error stop "maybe_payload_t%get: no payload" 
        get = self%payload_
    end procedure

end submodule
