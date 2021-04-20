module application_m
  use dag_interface, only : dag_t
  use task_item_m, only : task_item_t
  implicit none

  private
  public :: application_t

  type application_t
    private
    type(dag_t) dag_
    type(task_item_t), allocatable :: tasks_(:)
  end type

  interface application_t

    module function construct(dag, tasks) result(application)
      implicit none
      type(dag_t), intent(in) :: dag
      type(task_item_t), intent(in) :: tasks(:)
      type(application_t) application
    end function

  end interface

end module application_m
