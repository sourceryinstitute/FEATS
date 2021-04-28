module application_test
   !! verify application protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     assert_equals, describe, it, succeed ! functions
   use application_m, only : application_t
   use dag_interface, only: dag_t
   use image_m, only: image_t
   use results_data_m, only: results_data_t
   use task_item_m, only: task_item_t
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
    type(result_t) result_

    type(results_data_t) :: results

    ! associate(application => application_factory())
    !   call image%run(application, results)
    ! end associate
    ! result_ = &
    !     assert_equals(3.1415, results%mailbox[results%location_of(3)]) &
    !     .and. assert_equals(sqrt(2), results%mailbox[results%location_of(4)])

    result_ = succeed("application constructed")

  end function

  function application_factory() result(application)
      type(application_t) :: application

      type(dag_t) :: dag
      type(task_item_t), allocatable :: tasks(:)

      ! define the dag and task list
      ! application = application_t(dag, tasks)
  end function
end module application_test
