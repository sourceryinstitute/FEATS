submodule(task_payload_map_m) task_payload_map_s
    use mailbox_m, only: mailbox
    implicit none

contains
    module procedure get_task_ids
        ids = self%task_ids_
    end procedure

    module procedure constructor
        if (size(task_ids) /= size(image_nums)) then
            error stop "task_payload_map_t%constructor: mismatched argument lengths"
        end if
        new_map%task_ids_   = task_ids
        new_map%image_nums_ = image_nums
    end procedure


    module procedure get_raw_payload
        associate(idx => findloc(array=self%task_ids_, value=taskid, dim=1))
            if (idx == 0) then
                if(present(key_error)) then
                    key_error = .true.
                else
                    error stop 'task_payload_map_t%get_raw_payload: requested task id not found'
                end if
            else
                associate( img => self%image_nums_(idx) )
                    pl = mailbox[img]%payloads(taskid)%payload_ ! work around gfortran bug: direcly access payload_
                end associate
            end if
        end associate
    end procedure

    module procedure get_string_payload
        character(len=1), allocatable :: raw(:)
        integer :: i
        if (present(key_error)) then
            call self%get_raw_payload(taskid, raw, key_error)
        else
            call self%get_raw_payload(taskid, raw)
        end if
        allocate(character(len=size(raw)) :: pl)
        do concurrent (i = 1 : size(raw))
            pl(i:i) = raw(i)
        end do
    end procedure

end submodule
