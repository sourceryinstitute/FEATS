module application_generator_m
    use application_m, only: application_t
    implicit none
    private
    public :: generate_application

contains
    function generate_application() result(application)
        type(application_t) :: application

        associate(unused => application)
        end associate
    end function
end module
