module application_m
  use dag_m, only : dag_t
  use task_item_m, only : task_item_t
  implicit none

  private
  public :: application_t

  type application_t
    !! A complete representation of an application that can be executed by FEATS
    private
    type(dag_t) dag_ !! Describes the dependencies between tasks
    type(task_item_t), allocatable :: tasks_(:) !! tasks to be executed
  contains
    private
    procedure, public :: dag
    procedure, public :: tasks
  end type

  interface application_t

    pure module function construct(dag, tasks) result(application)
      implicit none
      type(dag_t), intent(in) :: dag
      type(task_item_t), intent(in) :: tasks(:)
      type(application_t) application
    end function

  end interface

  interface

    pure module function dag(self)
      implicit none
      class(application_t), intent(in) :: self
      type(dag_t) :: dag
    end function

    pure module function tasks(self)
      implicit none
      class(application_t), intent(in) :: self
      type(task_item_t), allocatable :: tasks(:)
    end function

  end interface

end module application_m
