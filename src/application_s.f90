submodule(application_m) application_s
  use dag_interface, only : dag_t
  use task_item_m, only : task_item_t
  use assertions_interface, only : assert
  implicit none

contains

  module procedure construct
    call assert(size(tasks)==dag%get_num_vertices(), "application(construct): size(tasks)==dag%get_num_vertices()")
    application%dag_ = dag
    application%tasks_ = tasks
  end procedure

end submodule application_s
