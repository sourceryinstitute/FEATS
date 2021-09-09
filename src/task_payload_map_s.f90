submodule(task_payload_map_m) task_payload_map_s
    implicit none

contains
    module procedure get_task_ids
        ids = self%task_ids_
    end procedure 

    module procedure get_payload
        associate(idx => findloc(array=self%task_ids_, value=taskid, dim=1))
            if (idx == 0) then
                p=maybe_payload_t()
            else
                p=maybe_payload_t(self%payloads_(idx))
            end if
        end associate
    end procedure

    module procedure constructor
        if (size(task_ids) /= size(payloads)) error stop "task_payload_map_t%constructor: mismatched arguments"
        new_map%task_ids_ = task_ids
        new_map%payloads_ = payloads
    end procedure
end submodule
