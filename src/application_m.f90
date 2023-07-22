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
    module procedure construct
  end interface
contains
    function construct(dag, tasks) result(application)
      implicit none
      type(dag_t), intent(in) :: dag
      type(task_item_t), intent(in) :: tasks(:)
      type(application_t) application

      application%dag_ = dag
      application%tasks_ = tasks
    end function

    pure function dag(self)
      implicit none
      class(application_t), intent(in) :: self
      type(dag_t) :: dag

      dag = self%dag_
    end function

    function tasks(self)
      implicit none
      class(application_t), intent(in) :: self
      type(task_item_t), allocatable :: tasks(:)

      tasks = self%tasks_
    end function
end module application_m
