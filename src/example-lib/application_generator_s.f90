submodule(application_generator_m) application_generator_s
  use vertex_m, only : vertex_t
  use iso_varying_string, only : varying_string, assignment(=)
  implicit none

contains

    module procedure application_generator

      type(dag_t) :: modules

      enum, bind(C)
        ! a topologically sorted enumeration of the items in the dependency tree
        enumerator :: &
          build_feats=1, app_generator_m, image_m, data_location_map_m, application_m, dag_m, task_item_m & 
         ,payload_m, task_m, mailbox_m, assert_m, application_s, payload_s, data_location_map_s, image_s, task_item_s &
         ,app_generator_s, vertex_m 
      end enum

      type(varying_string), allocatable :: name_list(:)

      integer, parameter :: num_vertices = size([ &
         build_feats,   app_generator_m, image_m, data_location_map_m, application_m, dag_m, task_item_m & 
        ,payload_m, task_m, mailbox_m, assert_m, application_s, payload_s, data_location_map_s, image_s, task_item_s &
        ,app_generator_s, vertex_m &
      ])

      allocate(name_list(num_vertices))

      name_list(build_feats)             = "build_feats"
      name_list(app_generator_m)         = "application_generator_m"
      name_list(image_m                ) = "image_m"
      name_list(data_location_map_m    ) = "data_location_map_m"
      name_list(application_m          ) = "application_m"
      name_list(dag_m          ) = "dag_m"
      name_list(task_item_m            ) = "task_item_m"
      name_list(payload_m              ) = "payload_m"
      name_list(task_m                 ) = "task_m"
      name_list(mailbox_m              ) = "mailbox_m"
      name_list(assert_m               ) = "assert_m"
      name_list(application_s          ) = "application_s"
      name_list(payload_s              ) = "payload_s"
      name_list(data_location_map_s    ) = "data_location_map_s"
      name_list(image_s                ) = "image_s"
      name_list(task_item_s            ) = "task_item_s"
      name_list(app_generator_s        ) = "application_generator_s"
      name_list(vertex_m       ) = "vertex_m"

      modules = dag_t([ &
         vertex_t( build_feats,        [app_generator_m, image_m],               name_list(build_feats)            ) &
        ,vertex_t(app_generator_m,     [integer::],                              name_list(app_generator_m)        ) &
        ,vertex_t(image_m,             [data_location_map_m, application_m],     name_list(image_m                )) &
        ,vertex_t(data_location_map_m, [integer::],                              name_list(data_location_map_m    )) &
        ,vertex_t(application_m,       [dag_m, task_item_m],             name_list(application_m          )) &
        ,vertex_t(dag_m,       [integer::],                              name_list(dag_m          )) &
        ,vertex_t(task_item_m,         [data_location_map_m, payload_m, task_m], name_list(task_item_m            )) &
        ,vertex_t(payload_m,           [integer::],                              name_list(payload_m              )) &
        ,vertex_t(task_m,              [data_location_map_m, payload_m],         name_list(task_m                 )) &
        ,vertex_t(mailbox_m,           [payload_m],                              name_list(mailbox_m              )) &
        ,vertex_t(assert_m,            [integer::],                              name_list(assert_m               )) &
        ,vertex_t(application_s,       [application_m, assert_m],                name_list(application_s          )) &
        ,vertex_t(payload_s,           [payload_m],                              name_list(payload_s              )) &
        ,vertex_t(data_location_map_s, [data_location_map_m],                    name_list(data_location_map_s    )) &
        ,vertex_t(image_s,             [image_m],                                name_list(image_s                )) &
        ,vertex_t(task_item_s,         [task_item_m],                            name_list(task_item_s            )) &
        ,vertex_t(app_generator_s,     [app_generator_m, vertex_m],      name_list(app_generator_s        )) &
        ,vertex_t(vertex_m,    [integer::],                              name_list(vertex_m       )) &
     ])
 
     block
       character(len=*), parameter :: base_name= 'feats-dependencies'
       character(len=*), parameter :: digraph_file = base_name // '.dot'
       character(len=*), parameter :: output_file = base_name // '.pdf'

       call modules%save_digraph(digraph_file, 'RL', 300)
       call execute_command_line('dot -Tpdf -o ' // output_file // ' ' // digraph_file)
       print *, new_line(''), " ----- application_generator(): module_depenencies DAG written to " // output_file
     end block

     block
       integer i
       application = application_t(modules, [(task_item_t(compile_or_link_t()) ,i=1,num_vertices)])
       print *, "----- application_generator(): application defined"
     end block

   end procedure

end submodule application_generator_s
