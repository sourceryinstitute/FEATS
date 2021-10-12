program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use feats_application_generator_m, only : generate_application
  use image_m, only: image_t
  use feats_result_map_m, only: feats_result_map_t
  implicit none

  type(image_t) :: image
  type(feats_result_map_t) :: results

  results = image%run(generate_application())
end program
