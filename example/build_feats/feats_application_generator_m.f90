module feats_application_generator_m
    use application_m, only: application_t
    use dag_m, only : dag_t
    use iso_varying_string, only: varying_string, operator(//), put_line, var_str
    use payload_m, only: payload_t, empty_payload
    use strff, only: to_string
    use task_payload_map_m, only: task_payload_map_t
    use task_m, only: task_t
    use task_item_m, only: task_item_t
    use vertex_m, only : vertex_t

    implicit none
    private
    public :: generate_application

    type, extends(task_t) :: compile_task_t
        type(varying_string) :: to_compile
    contains
        procedure :: execute => compile_task_execute
    end type
contains
    function generate_application() result(application)
        type(application_t) :: application

        character(len=*), parameter :: longest_name = "app_generator_m"
        character(len=len(longest_name)), parameter :: names(*) = &
            [ character(len=len(longest_name)) :: "assert_m" &
            , "dag_m" &
            , "payload_m" &
            , "compile_m" &
            , "data_loc_map_m" &
            , "task_m" &
            , "task_item_m" &
            , "app_m" &
            , "app_generator_m" &
            , "image_m" &
            , "main" &
            , "task_item_s" &
            , "compile_s" &
            , "app_generator_s" &
            , "data_loc_map_s" &
            , "payload_s" &
            , "app_s" &
            , "mailbox_m" &
            , "image_s" &
            , "final_task_m" &
            , "final_task_s" &
            ]
        associate( &
              assert_m        => findloc(names, "assert_m",        dim=1) &
            , dag_m           => findloc(names, "dag_m",           dim=1) &
            , payload_m       => findloc(names, "payload_m",       dim=1) &
            , compile_m       => findloc(names, "compile_m",       dim=1) &
            , data_loc_map_m  => findloc(names, "data_loc_map_m",  dim=1) &
            , task_m          => findloc(names, "task_m",          dim=1) &
            , task_item_m     => findloc(names, "task_item_m",     dim=1) &
            , app_m           => findloc(names, "app_m",           dim=1) &
            , app_generator_m => findloc(names, "app_generator_m", dim=1) &
            , image_m         => findloc(names, "image_m",         dim=1) &
            , main            => findloc(names, "main",            dim=1) &
            , task_item_s     => findloc(names, "task_item_s",     dim=1) &
            , compile_s       => findloc(names, "compile_s",       dim=1) &
            , app_generator_s => findloc(names, "app_generator_s", dim=1) &
            , data_loc_map_s  => findloc(names, "data_loc_map_s",  dim=1) &
            , payload_s       => findloc(names, "payload_s",       dim=1) &
            , app_s           => findloc(names, "app_s",           dim=1) &
            , mailbox_m       => findloc(names, "mailbox_m",       dim=1) &
            , image_s         => findloc(names, "image_s",         dim=1) &
            , final_task_m    => findloc(names, "final_task_m",    dim=1) &
            , final_task_s    => findloc(names, "final_task_s",    dim=1) &
        )

            block
                character(len=*),           parameter :: external_ = 'shape=square,fillcolor="green",style=filled'
                character(len=*),           parameter :: root      = 'shape=circle,fillcolor="white",style=filled'
                character(len=*),           parameter :: branch    = 'shape=square,fillcolor="SlateGray1",style=filled'
                character(len=len(branch)), parameter :: leaf      = 'shape=circle,fillcolor="cornsilk",style=filled'
                type(dag_t) feats
                integer i
                type(varying_string) name_string(size(names))
                type(task_item_t), allocatable :: tasks(:)

                name_string = var_str(names)

                feats = &
                    dag_t([ &
                          vertex_t([integer::],                            name_string(assert_m),        var_str(external_)) &
                        , vertex_t([integer:: ],                           name_string(dag_m),           var_str(external_)) &
                        , vertex_t([integer::],                            name_string(payload_m),       var_str(leaf)     ) &
                        , vertex_t([integer:: ],                           name_string(compile_m),       var_str(leaf)     ) &
                        , vertex_t([integer::],                            name_string(data_loc_map_m),  var_str(leaf)     ) &
                        , vertex_t([payload_m],                            name_string(task_m),          var_str(branch)   ) &
                        , vertex_t([task_m],                               name_string(task_item_m),     var_str(leaf)     ) &
                        , vertex_t([dag_m, task_item_m],                   name_string(app_m),           var_str(branch)   ) &
                        , vertex_t([app_m, dag_m, task_item_m, compile_m], name_string(app_generator_m), var_str(branch)   ) &
                        , vertex_t([app_m, data_loc_map_m],                name_string(image_m),         var_str(branch)   ) &
                        , vertex_t([app_generator_m, image_m],             name_string(main),            var_str(root)     ) &
                        , vertex_t([task_item_m],                          name_string(task_item_s),     var_str(root)     ) &
                        , vertex_t([compile_m],                            name_string(compile_s),       var_str(branch)   ) &
                        , vertex_t([app_generator_m],                      name_string(app_generator_s), var_str(root)     ) &
                        , vertex_t([data_loc_map_m],                       name_string(data_loc_map_s),  var_str(root)     ) &
                        , vertex_t([payload_m],                            name_string(payload_s),       var_str(root)     ) &
                        , vertex_t([app_m, assert_m],                      name_string(app_s),           var_str(root)     ) &
                        , vertex_t([payload_m],                            name_string(mailbox_m),       var_str(branch)   ) &
                        , vertex_t([image_m, mailbox_m],                   name_string(image_s),         var_str(root)     ) &
                        , vertex_t([data_loc_map_m, payload_m, task_m],    name_string(final_task_m),    var_str(branch)   ) &
                        , vertex_t([final_task_m],                         name_string(final_task_s),    var_str(root)     ) &
                    ])
                tasks = [(task_item_t(compile_task_t(name_string(i))), i = 1, size(names))]
                application = application_t(feats, tasks)
            end block
        end associate
    end function

    function compile_task_execute(self, arguments) result(output)
        class(compile_task_t), intent(in) :: self
        type(payload_t), intent(in) :: arguments(:)
        type(payload_t) :: output

        real :: rand

        call put_line( &
                "Compiling: " // self%to_compile &
                // " on image number: " // to_string(this_image()))
        call random_number(rand)
        call sleep(int(rand * 10))
        call put_line("Finished Compiling: " // self%to_compile)

        output = empty_payload()
    end function
end module
