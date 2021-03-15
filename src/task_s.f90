submodule(task_m) task_s
  implicit none

contains

  module procedure do_work
    !! execute the a subroutine if the task defines one and report result
    if (present(verbose)) then
      if (this%identifier_ == no_work) then
        print *, "Image", this_image(), "does no work"
      else
        print *, "Image", this_image(), "does work on task", this%identifier_
      end if
    end if
  end procedure

end submodule task_s
