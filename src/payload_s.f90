submodule(payload_m) payload_s
    implicit none
contains
    module procedure from_raw
        new_payload%payload_ = payload
    end procedure

    module procedure from_string
        integer :: i
        new_payload%payload_ = transfer(payload,[integer(1)::])
    end procedure

    module procedure raw_payload
        if (allocated(self%payload_)) then
            raw_payload = self%payload_
        else
            raw_payload = [integer(1)::]
        end if
    end procedure

    module procedure string_payload
        integer :: i
        string_payload=transfer(self%payload_,string_payload)
    end procedure

end submodule
