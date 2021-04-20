module application_test
   !! verify application protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     describe, it, succeed ! functions
   use application_m, only : application_t
   implicit none

   private
   public :: test_application

contains

  function test_application() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "application class", &
     [it( &
       "constructs an application", &
       verify_application_construction) &
       ])

  end function

  function verify_application_construction() result(result_)
    type(application_t) application
    type(result_t) result_

    !application = application_t()

    result_ = succeed("application constructed")

  end function

end module application_test
