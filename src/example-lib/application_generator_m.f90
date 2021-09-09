module application_generator_m
  !! Define a module_dependencies directed acyclic graph (DAG) 
  !! to represent the feats module dependency tree execute
  !! tasks at each vertex in the tree.

    use application_m, only: application_t
    use dag_m, only : dag_t
    use task_item_m, only : task_item_t
    use compile_or_link_m, only : compile_or_link_t
    use iso_fortran_env, only : error_unit
    implicit none
    private
    public :: application_generator

    interface

      module function application_generator() result(application)
        implicit none
        type(application_t) :: application
      end function

    end interface

end module application_generator_m
