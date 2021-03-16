module image_test
   !! verify image set-up & protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     describe, it, succeed ! functions
   use scheduler_m, only : scheduler_t
   use compute_m, only : compute_t
   use iso_fortran_env, only : event_type
   implicit none

   private
   public :: test_image

contains

  function test_image() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "image class", &
     [it( &
       "set_up completes", &
       verify_image_set_up) &
       ])

  end function

  function verify_image_set_up() result(result_)
    !!  Test the setup of scheduler and compute images
    type(scheduler_t) scheduler
    type(result_t) result_

    call scheduler%set_up()
    call scheduler%distribute_initial_tasks()

    result_ = succeed(merge("scheduler assigned tasks", "compute image did task  ", scheduler%is_this_image()))

  end function

end module image_test
