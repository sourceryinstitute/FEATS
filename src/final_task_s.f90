submodule(final_task_m) final_task_s
    implicit none
contains
    module procedure execute
    end procedure

    module procedure is_final_task
        is_final_task = .true.
    end procedure
end submodule
