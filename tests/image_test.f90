module image_test
   !! verify image protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     describe, it, succeed ! functions
   use image_m, only : image_t
   implicit none

   private
   public :: test_image

contains

  function test_image() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "image class", &
     [it( &
       "distributes and does initial tasks", &
       verify_image_set_up) &
       ])

  end function

  function verify_image_set_up() result(result_)
    !!  Test the setup of scheduler and compute images
    type(image_t) image
    type(result_t) result_

    if (image%is_scheduler()) then
      call image%distribute_initial_tasks
      result_ = succeed("scheduler assigned initial tasks")
    else
      call image%wait_do_task_notify_ready
      result_ = succeed("compute image did its initial task")
    end if
  end function

end module image_test
