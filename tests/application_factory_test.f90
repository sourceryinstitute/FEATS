module application_factory_test
   !! verify application_factory protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     describe, it, succeed ! functions
   use application_factory_m, only : application_factory_t
   use application_m, only : application_t
   implicit none

   private
   public :: test_application_factory

contains

  function test_application_factory() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "application_factory class", &
     [it( &
       "constructs an application", &
       verify_application_construction) &
       ])

  end function

  function verify_application_construction() result(result_)
    type(application_t) application
    type(application_factory_t) application_factory
    type(result_t) result_

    application = application_factory%factory_method()

    result_ = succeed("application constructed")

  end function

end module application_factory_test
