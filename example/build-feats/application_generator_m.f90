module application_generator_m
  !! Define a module_dependencies directed acyclic graph (DAG) 
  !! to represent the following dependency tree:
  !!
  !! build_feats (main)
  !! |-- application_generator_m
  !! |-- image_m
  !!     |-- data_location_map_m
  !!     |-- application_m
  !!         |-- dag_interface
  !!         |-- task_item_m
  !!             |-- data_location_map_m
  !!             |-- payload_m
  !!             |-- task_m
  !!                 |-- data_location_map_m
  !!                 |-- payload_m
  !!         
  !! mailbox_m
  !! |-- payload_m
  !! 
  !! application_s
  !! |-- application_m
  !! |-- assertions_interface (external)
  !! 
  !! payload_s
  !! |-- payload_m
  !! 
  !! data_location_map_s
  !! |-- data_location_map_m
  !! 
  !! image_s
  !! |-- image_m
  !! 
  !! task_item_s
  !! |-- task_item_m
    use application_m, only: application_t
    use dag_interface, only : dag_t
    use task_item_m, only : task_item_t
    use iso_fortran_env, only : error_unit
    implicit none
    private
    public :: application_generator

contains
    function application_generator() result(application)
      type(application_t) :: application
      type(dag_t) :: module_dependencies

      enum, bind(C)
        ! a topologically sorted enumeration of the items in the dependency tree
        enumerator :: &
        build_feats=1, application_generator_m, image_m, data_location_map_m, application_m, dag_interface, task_item_m, & 
        payload_m, task_m, mailbox_m, assertions_interface, application_s, payload_s, data_location_map_s, image_s, task_item_s
      end enum

      integer, parameter :: num_vertices = size([ &
        build_feats,   application_generator_m, image_m, data_location_map_m, application_m, dag_interface, task_item_m, & 
        payload_m, task_m, mailbox_m, assertions_interface, application_s, payload_s, data_location_map_s, image_s, task_item_s &
      ])

      call module_dependencies%set_vertices(num_vertices)
      call module_dependencies%set_edges(build_feats, [application_generator_m, image_m])   
      call module_dependencies%set_edges(image_m, [data_location_map_m, application_m])
      call module_dependencies%set_edges(application_m, [dag_interface, task_item_m])
      call module_dependencies%set_edges(task_item_m, [data_location_map_m, payload_m, task_m])
      call module_dependencies%set_edges(task_m, [data_location_map_m, payload_m])
      call module_dependencies%set_edges(mailbox_m, [payload_m])
      call module_dependencies%set_edges(application_s, [application_m, assertions_interface])
      call module_dependencies%set_edges(payload_s, [payload_m])
      call module_dependencies%set_edges(data_location_map_s, [data_location_map_m])
      call module_dependencies%set_edges(image_s, [image_m])
      call module_dependencies%set_edges(task_item_s, [task_item_m])
 
     block
       integer i
       character(len=*),                   parameter :: non_leaf_color = 'shape=square,fillcolor="SlateGray1",style=filled'
       character(len=len(non_leaf_color)), parameter :: leaf_color     = 'shape=circle,fillcolor="cornsilk",style=filled'
       integer, parameter :: leaf_nodes(*) = &
         [application_generator_m, data_location_map_m, dag_interface, payload_m, &
         assertions_interface, application_s, payload_s, data_location_map_s, image_s, task_item_s]
       
       do i = 1, num_vertices
         associate(node_color => merge(leaf_color, non_leaf_color, any(i==leaf_nodes)))
           call module_dependencies%set_vertex_info(i, attributes = node_color)
         end associate
       end do

     end block

     block
       character(len=*), parameter :: path = './output/'
       character(len=*), parameter :: base_name= 'feats-dependencies'
       character(len=*), parameter :: digraph_file = path // base_name // '.dot'
       character(len=*), parameter :: output_file_arg = '-o ' // path // base_name // '.pdf'

       print *, &
         new_line('a'), &
         "build-feats example: uncomment the save_digraph type-bound procedure call to see the assertion failure", &
         new_line('a')
    
!       call module_dependencies%save_digraph(digraph_file, 'RL', 300)

!       call execute_command_line('dot -Tpdf ' // output_file_arg // ' ' // digraph_file)
     end block

!      application = application_t(module_dependencies, [ task_item_t :: ])

    end function

end module
