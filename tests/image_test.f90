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
    integer image

    call me%set_up()
    if (me%scheduler()) then
      do image=2,num_images()
        call me%scheduler_assigns_task(compute_image=image)
      end do
    else
      call me%wait_do_task_notify_ready
    end if

    result_ = succeed(merge("scheduler assigned tasks", "compute image did task  ", me%scheduler()))

  end function

end module image_test
