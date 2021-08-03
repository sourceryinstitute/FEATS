submodule(task_m) task_s
    implicit none
contains
    module procedure is_final_task
        is_final_task = .false.
    end procedure
end submodule
