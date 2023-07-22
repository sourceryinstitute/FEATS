program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use feats_application_generator_m, only : generate_application
  use image_m, only: image_t
  implicit none

  type(image_t) :: image

  call image%run(generate_application())
end program
