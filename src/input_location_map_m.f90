module input_location_map_m
    implicit none
    private
    public :: input_location_map_t

    type :: input_location_map_t
        private
        integer, allocatable :: task_numbers(:), image_numbers(:)
    contains
        private
        procedure, public :: location_of
    end type

    interface input_location_map_t
        pure module function constructor(task_numbers, image_numbers) result(input_location_map)
            implicit none
            integer, intent(in) :: task_numbers(:), image_numbers(:)
            type(input_location_map_t) :: input_location_map
        end function
    end interface

    interface
        pure module function location_of(self, task)
            implicit none
            class(input_location_map_t), intent(in) :: self
            integer, intent(in) :: task
            integer :: location_of
        end function
    end interface
end module
