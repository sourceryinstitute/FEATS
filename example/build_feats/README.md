Proof of Concept
----------------
This directory's [main program](./main.f90) defines an application in the form
of a FEATS `application_t` object containing 

1. A directed acyclic graph (DAG) encapsulated in a `dag_t` object provided by 
   the [dag] library and
2. An array of FEATS `task_t` objects capturing information about tasks that
   that the scheduler image assigns to compute images in an order that respects
   the dependencies described in the DAG.

The DAG in this example represents the module dependencies within the FEATS
source code as described in the diagram at the bottom of this page.  

The motivating use case is enabling the Fortran Package Manager ([`fpm`]) to
perform parallel builds.  In this proof concept, each task simply sleeps to pass
a randomly chosen period of time. (No actual software compiling happens.)  See
below for the command used to run this example with present working directory
set to the top directory in the FEATS source tree.  Also below is sample output.
The task completion order and image/task pairings will vary from one execution
of the first command to the next execution of the same command.

```
fpm run --example build_feats --compiler caf --runner "cafrun -n 4"
Executing task number: 2 on image number: 3
Executing task number: 1 on image number: 4
Executing task number: 3 on image number: 2
Compiling: dag_m          
Executing task number: 4 on image number: 3
Compiling: assert_m       
Executing task number: 5 on image number: 4
Compiling: payload_m      
Executing task number: 6 on image number: 2
Compiling: data_loc_map_m 
Compiling: task_m         
Executing task number: 15 on image number: 2
Executing task number: 7 on image number: 4
Compiling: compile_m      
Executing task number: 13 on image number: 3
Compiling: task_item_m    
Executing task number: 8 on image number: 4
Compiling: app_m          
Executing task number: 9 on image number: 4
Compiling: data_loc_map_s 
Executing task number: 10 on image number: 2
Compiling: compile_s      
Executing task number: 12 on image number: 3
Compiling: app_generator_m
Executing task number: 14 on image number: 4
Compiling: task_item_s    
Executing task number: 16 on image number: 3
Compiling: image_m        
Executing task number: 11 on image number: 2
Compiling: app_generator_s
Executing task number: 17 on image number: 4
Compiling: main           
Executing task number: 18 on image number: 2
Compiling: payload_s      
Executing task number: 20 on image number: 3
Compiling: mailbox_m      
Compiling: app_s          
Executing task number: 19 on image number: 4
Compiling: image_s        
Compiling: final_task_m   
Executing task number: 21 on image number: 4
Compiling: final_task_s   
```

![feats-dependencies](https://user-images.githubusercontent.com/13108868/133311851-721b7cda-1d10-4ee1-a51d-6169ca624839.png)

[`fpm`]: https://github.com/fortran-lang/fpm
[dag]: https://github.com/sourceryinstitute/dag
