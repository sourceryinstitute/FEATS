module application_factory_m
    use application_m, only: application_t
    implicit none
    private
    public :: application_factory

    interface
        module function application_factory() result(application)
            implicit none
            type(application_t) :: application
        end function
    end interface
end module
