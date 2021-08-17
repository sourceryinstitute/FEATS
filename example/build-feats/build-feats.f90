program build_feats
  !! Run an application created from a directed acyclic graph (DAG) 
  !! describing an early version of the feats internal dependency tree
  !! and an empty task_item array.
  use application_generator_m, only : application_generator
  use image_m, only: image_t
  implicit none

  associate(image => image_t())
   associate(results => image%run(application_generator()))
   end associate
  end associate
end program
