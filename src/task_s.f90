submodule(task_m) task_s
  implicit none

contains

  module procedure do_work
    !! execute  a subroutine if the task defines one and report result;
    !! otherwise, report that no work is being done
    if (present(verbose)) then
      if (this%identifier_ == no_work) then
        print *, "Image", this_image(), "does no work"
      else
        print *, "Image", this_image(), "does work on task", this%identifier_
        ! Write code here to call the task's subroutine
      end if
    end if
  end procedure

end submodule task_s
