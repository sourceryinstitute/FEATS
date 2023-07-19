submodule(application_m) application_s
  implicit none
contains

  module procedure construct
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
