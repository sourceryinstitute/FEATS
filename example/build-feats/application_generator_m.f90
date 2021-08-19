module application_generator_m
    use application_m, only: application_t
    use dag_interface, only : dag_t
    use task_item_m, only : task_item_t
    implicit none
    private
    public :: application_generator

contains
    function application_generator() result(application)
        type(application_t) :: application
        type(dag_t) :: module_dependencies

        ! To Do
        ! -----
        !
        ! Insert code to define module_dependencies to represent 
        ! the following dependency tree:
        !
        ! build_feats (main)
        ! |-- application_generator_m
        ! |-- image_m
        !     |-- data_location_map_m
        !     |-- application_m
        !         |-- dag_interface
        !         |-- task_item_m
        !             |-- data_location_map_m
        !             |-- payload_m
        !             |-- task_m
        !                 |-- data_location_map_m
        !                 |-- payload_m
        !         
        ! mailbox_m
        ! |-- payload_m
        ! 
        ! application_s
        ! |-- application_m
        ! |-- assertions_interface (external)
        ! 
        ! payload_s
        ! |-- payload_m
        ! 
        ! data_location_map_s
        ! |-- data_location_map_m
        ! 
        ! image_s
        ! |-- image_m
        ! 
        ! task_item_s
        ! |-- task_item_m
        
        application = application_t(module_dependencies, [ task_item_t :: ])
    end function
end module
