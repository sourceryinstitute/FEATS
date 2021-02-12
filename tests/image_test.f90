module image_test
   !! verify image set-up & protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     describe, it, succeed ! functions
   use image_m, only : image_t
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
    !!  Test that the executing team has only one scheduler
    type(image_t) me
    type(result_t) result_

    call me%set_up()

    result_ = succeed("")

  end function

end module image_test
