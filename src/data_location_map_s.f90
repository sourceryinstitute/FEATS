submodule(data_location_map_m) data_location_map_s
    implicit none
contains
    module procedure constructor
        if (size(task_numbers) /= size(image_numbers)) &
            error stop 'Inconsistent arguments passed to FEATS data_location_map_t%constructor'
        data_location_map%task_numbers = task_numbers
        data_location_map%image_numbers = image_numbers
    end procedure

    module procedure location_of
        associate(which_task => findloc(array=self%task_numbers, value=task, dim=1))
            if (which_task == 0) then
                error stop "task not present in this map"
            else
                location_of = self%image_numbers(which_task)
            end if
        end associate
    end procedure
end submodule
