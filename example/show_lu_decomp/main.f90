program show_lu_decomp
    use dag_m, only: dag_t
    use iso_varying_string, only: var_str
    use vertex_m, only: vertex_t

    implicit none

    character(len=*), parameter :: root   = 'shape=circle,fillcolor="white",style=filled'
    character(len=*), parameter :: branch = 'shape=square,fillcolor="SlateGray1",style=filled'
    character(len=*), parameter :: leaf   = 'shape=circle,fillcolor="cornsilk",style=filled'
    type(vertex_t), allocatable :: vertices(:)
    type(dag_t) :: dag
    integer :: num_tasks, step, row, matrix_size, latest_matrix, task_base, reconstruction_step, i

    do matrix_size = 1, 5
        num_tasks = 4 + (matrix_size-1)*2 + sum([((matrix_size-step)*3, step=1,matrix_size-1)])
        allocate(vertices(num_tasks))

        vertices(1) = vertex_t([integer::], var_str("initial matrix"), var_str(leaf))
        vertices(2) = vertex_t([1], var_str("print matrix"), var_str(root))
        do step = 1, matrix_size-1
            do row = step+1, matrix_size
                latest_matrix = 1 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) ! reconstructed matrix from last step
                task_base = sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) + 3*(row-(step+1))
                vertices(3+task_base) = vertex_t([latest_matrix], var_str("calculate factor"), var_str(branch))
                vertices(4+task_base) = vertex_t([latest_matrix, 3+task_base], var_str("row multiply"), var_str(branch))
                vertices(5+task_base) = vertex_t([latest_matrix, 4+task_base], var_str("row subtract"), var_str(branch))
            end do
            reconstruction_step = 3 + sum([(3*(matrix_size-i), i = 1, step)]) + 2*(step-1)
            vertices(reconstruction_step) = vertex_t( &
                [ 1 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) &
                , [(5 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) + 3*(row-(step+1)) &
                    , row=step+1, matrix_size)] &
                ], &
                var_str("reconstruct matrix"), var_str(branch)) ! depends on previous reconstructed matrix and just subtracted rows
            ! print the just reconstructed matrix
            vertices(reconstruction_step+1) = vertex_t([reconstruction_step], var_str("print matrix"), var_str(root))
        end do
        vertices(num_tasks-1) = vertex_t( &
            [([(3 + sum([(3*(matrix_size-i), i = 1, step-1)]) + 2*(step-1) + 3*(row-(step+1)) &
                , row=step+1, matrix_size)] &
                , step=1, matrix_size-1)] &
            , var_str("back substitute"), var_str(branch)) ! depends on all "factors"
        vertices(num_tasks) = vertex_t([num_tasks-1], var_str("print matrix"), var_str(root))

        dag = dag_t(vertices)
        deallocate(vertices)

        block
            character(len=15) :: dotfile_name
            integer :: funit

            write(dotfile_name, '("lu_decomp_", I1, ".dot")') matrix_size
            open(newunit = funit, file = dotfile_name, status="replace")
            write(funit, "(A)") dag%graphviz_digraph()
            close(funit)
        end block
    end do
end program