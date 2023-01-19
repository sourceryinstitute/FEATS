submodule(payload_m) payload_s
    implicit none
contains
    module procedure from_raw
        integer :: incoming_payload_size

        incoming_payload_size = size(payload)
        if (incoming_payload_size > MAX_PAYLOAD_SIZE) then
            new_payload%payload_size = MAX_PAYLOAD_SIZE
            new_payload%payload_ = payload(1:MAX_PAYLOAD_SIZE)
        else
            new_payload%payload_size = incoming_payload_size
            new_payload%payload_(1:incoming_payload_size) = payload
        end if
    end procedure

    module procedure from_string
        integer :: string_length, length_as_payload, end_of_transmittable_string

        string_length = len(payload)
        length_as_payload = storage_size(char(1))/storage_size(1)*string_length + 1
        if (length_as_payload > MAX_PAYLOAD_SIZE) then
        block
            integer, parameter :: end_of_transmittable_string = storage_size(1)/storage_size(char(1))*(MAX_PAYLOAD_SIZE-1)

            new_payload%payload_size = MAX_PAYLOAD_SIZE
            new_payload%payload_ =  &
                    [ end_of_transmittable_string &
                    , transfer(payload(1:end_of_transmittable_string), new_payload%payload_(2:)) &
                    ]
        end block
        else
            new_payload%payload_size = length_as_payload
            new_payload%payload_ = &
                    [ string_length &
                    , transfer(payload, new_payload%payload_(1:length_as_payload)) &
                    ]
        end if
    end procedure

    module procedure empty_payload
    end procedure

    module procedure raw_payload
        raw_payload = self%payload_(1:self%payload_size)
    end procedure

    module procedure string_payload
        if (self%payload_size > 0) then
            allocate(character(len=self%payload_(1)) :: string_payload)
            if (self%payload_(1)> 0) &
                    string_payload = transfer( &
                            self%payload_(2:self%payload_size), string_payload)
        else
            allocate(character(len=0) :: string_payload)
        end if
    end procedure
end submodule
