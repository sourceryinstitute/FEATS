submodule(payload_m) payload_s
    implicit none
contains
    module procedure from_raw
        new_payload%payload_ = payload
    end procedure

    module procedure from_string
        new_payload = payload_t([len(payload), transfer(payload,[integer::])])
    end procedure

    module procedure empty_payload
        empty_payload%payload_  = [integer::]
    end procedure

    module procedure raw_payload
        if (allocated(self%payload_)) then
            raw_payload = self%payload_
        else
            raw_payload = [integer::]
        end if
    end procedure

    module procedure string_payload
        if (allocated(self%payload_)) then
            if (size(self%payload_) > 0) then
                allocate(character(len=self%payload_(1)) :: string_payload)
                if (len(string_payload) > 0) &
                    string_payload = transfer(self%payload_(2:),string_payload)
            else
                allocate(character(len=0) :: string_payload)
            end if
        else
            allocate(character(len=0) :: string_payload)
        end if
    end procedure

end submodule
