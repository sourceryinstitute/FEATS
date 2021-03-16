module task_test
   !! verify task set-up & protocol for task assignment, completion, & readiness notification
   use vegetables, only: &
     result_t, test_item_t, & ! types
     describe, it, succeed ! functions
   use task_m, only : task_t
   use iso_fortran_env, only : event_type
   implicit none

   private
   public :: test_task

contains

  function test_task() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "task class", &
     [it( &
       "can be constructed and assignmed", &
       verify_task_construction) &
       ])

  end function

  function verify_task_construction() result(result_)
    !!  Test that a task can be constructed
    type(task_t) task
    type(result_t) result_

    task = task_t()

    result_ = succeed("task constructed")

  end function

end module task_test
