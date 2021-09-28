program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use application_generator_m, only : generate_application
  use image_m, only: image_t
  use task_payload_map_m, only: task_payload_map_t
  implicit none

  type(image_t) :: image
  type(task_payload_map_t) :: results

  results = image%run(generate_application())
end program
