submodule(payload_m) payload_s
    implicit none
contains
    module procedure constructor
        new_payload%payload_ = payload_
    end procedure

    module procedure payload
        payload = self%payload_
    end procedure
end submodule
