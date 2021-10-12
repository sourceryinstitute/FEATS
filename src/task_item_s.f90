submodule(task_item_m) task_item_s
  !! define tasks for compute images to complete
  implicit none

contains

  module procedure execute
      output = self%task%execute(arguments)
  end procedure

  module procedure constructor
      new_task_item%task = task
  end procedure

  module procedure is_final_task
      is_final_task = self%task%is_final_task()
  end procedure

end submodule task_item_s
