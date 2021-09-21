program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use application_m, only: application_t
  use application_generator_m, only : generate_application
  use image_m, only: image_t
  use task_payload_map_m, only: task_payload_map_t
  implicit none

  type(application_t) :: application
  type(image_t) :: image
  type(task_payload_map_t) :: results

  application = generate_application()
  results = image%run(application)
end program
