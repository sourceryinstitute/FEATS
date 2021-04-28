program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use application_factory_m, only : application_factory
  use image_m, only: image_t
  use results_data_m, only: results_data_t
  implicit none

  type(image_t) :: image
  type(results_data_t) :: results

  associate(application => application_factory())
    call image%run(application, results)
    ! can access results if needed using
    ! results%mailbox(task)[results%location_of(task)]
  end associate
end program
