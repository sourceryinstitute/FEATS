module application_generator_m
  !! Define a module_dependencies directed acyclic graph (DAG) 
  !! to represent the following dependency tree:
  !!
  !! build_feats (main)
  !! |-- {image_m, application_generator_m}
  !!      ^
  !!      |-- {application_m, data_location_map_m}
  !!           ^
  !!           |-- {task_item_m, dag_interface, task_item_m}
  !!                ^
  !!                |-- {task_m, data_location_map_m, payload_m, 
  !!                     ^
  !!                     |-- { data_location_map_m, payload_m}
  !!         
  !! mailbox_m - {payload_m}
  !! 
  !! application_s - {application_m, assert_m (external)}
  !! 
  !! payload_s - {payload_m}
  !! 
  !! data_location_map_s - {data_location_map_m}
  !! 
  !! image_s - {image_m}
  !! 
  !! task_item_s - {task_item_m}

    use application_m, only: application_t
    use dag_interface, only : dag_t
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
