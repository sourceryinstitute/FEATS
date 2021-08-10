module data_location_map_m
    implicit none
    private
    public :: data_location_map_t

    type :: data_location_map_t
        !! A mapping that describes for given task ID, on which image was it executed
        !!
        !! This provides a convenient way for task to inquire about which images its inputs are stored on
        private
        integer, allocatable :: task_numbers(:), image_numbers(:)
    contains
        private
        procedure, public :: location_of
    end type

    interface data_location_map_t
        pure module function constructor(task_numbers, image_numbers) result(data_location_map)
            implicit none
            integer, intent(in) :: task_numbers(:), image_numbers(:)
            type(data_location_map_t) :: data_location_map
        end function
    end interface

    interface
        pure module function location_of(self, task)
            implicit none
            class(data_location_map_t), intent(in) :: self
            integer, intent(in) :: task
            integer :: location_of
        end function
    end interface
end module
