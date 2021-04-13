module image_test
   !! verify image protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     describe, it, succeed ! functions
   use image_m, only : image_t
   use task_item_m, only : task_item_t
   use task_m, only : task_t
   implicit none

   private
   public :: test_image

   type, extends(task_t) :: test_task_t
   contains
     procedure :: do_work
   end type

contains

  function test_image() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "image class", &
     [it( &
       "distributes and does initial tasks", &
       verify_initial_task_distribution) &
       ])

  end function

  function verify_initial_task_distribution() result(result_)
    !!  Test the setup of scheduler and compute images
    type(image_t) image
    type(result_t) result_
    integer i

    call image%distribute_and_do_initial_tasks([(task_item_t(test_task_t()), i = 1, num_images())])

    result_ = succeed("initial tasks distributed and done")

  end function

  subroutine do_work(self)
    class(test_task_t), intent(in) :: self
  end subroutine

end module image_test
