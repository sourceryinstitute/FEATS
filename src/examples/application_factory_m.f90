module application_factory_m
  use application_m, only : application_t
  implicit none

  private
  public :: application_factory_t

  type application_factory_t
  contains
    procedure, nopass :: factory_method
  end type

  interface

    module function factory_method() result(application)
      implicit none
      type(application_t) application
    end function

  end interface

end module
