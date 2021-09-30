module legacy_m
    implicit none
    private
    public :: square, four_a_c
contains
    function square(b)
        real, intent(in) :: b
        real :: square

        square = b*b
    end function

    function four_a_c(a, c)
        real, intent(in) :: a, c
        real :: four_a_c

        four_a_c = 4*a*c
    end function
end module
