submodule(application_m) application_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    call assert(size(tasks)==dag%num_vertices(), "application(construct): size(tasks)==dag%get_num_vertices()")
    application%dag_ = dag
    application%tasks_ = tasks
  end procedure

  module procedure dag
    dag = self%dag_
  end procedure

  module procedure tasks
    tasks = self%tasks_
  end procedure

end submodule application_s
