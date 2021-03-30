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
       verify_initial_task_distribution) &
       ])

  end function

  function verify_initial_task_distribution() result(result_)
    !!  Test the setup of scheduler and compute images
    type(image_t) image
    type(result_t) result_

    call image%distribute_and_do_initial_tasks

    result_ = succeed("initial tasks distributed and done")


  end function

end module image_test
