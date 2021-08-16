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
    end procedure
end submodule
