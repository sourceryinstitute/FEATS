program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use application_generator_m, only : generate_application
  use image_m, only: image_t
  use mailbox_m, only: mailbox
  implicit none

  type(image_t) :: image

  associate(application => generate_application())
    associate(results => image%run(application))
      ! can access results if needed using
      ! mailbox(task)[results%location_of(task)]
    end associate
  end associate
end program
