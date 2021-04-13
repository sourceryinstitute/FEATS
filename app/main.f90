program main
  !! Framework for Extensible Asynchronous Task Scheduling
  use application_factory_m, only : application_factory_t
  implicit none
  type(application_factory_t) application_factory

  associate(application => application_factory%factory_method())

  end associate

end program
