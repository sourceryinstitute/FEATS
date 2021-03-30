submodule(task_item_m) task_item_s
  !! define tasks for compute images to complete
  implicit none

contains

  module procedure do_work
    call self%task%do_work
  end procedure

  module procedure constructor
    new_task_item%task = task
  end procedure

end submodule task_item_s
