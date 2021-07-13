module application_factory_m
    use application_m, only: application_t
    implicit none
    private
    public :: application_factory

contains
    function application_factory() result(application)
        type(application_t) :: application
        
        associate(unused => application)
        end associate
    end function
end module
