module results_data_m
    use input_location_map_m, only: input_location_map_t
    use payload_item_m, only: payload_item_t
    implicit none
    private
    public :: results_data_t

    type :: results_data_t
        private
        type(input_location_map_t) :: data_locations
        type(payload_item_t), public, allocatable :: mailbox(:)[:]
    contains
        private
        procedure, public :: define
        procedure, public :: location_of
    end type

    interface
        module subroutine define(self, data_locations, mailbox)
            implicit none
            class(results_data_t), intent(inout) :: self
            type(input_location_map_t), intent(in) :: data_locations
            type(payload_item_t), intent(in) :: mailbox(:)[*]
        end subroutine

        module pure function location_of(self, task)
            implicit none
            class(results_data_t), intent(in) :: self
            integer, intent(in) :: task
            integer :: location_of
        end function
    end interface
end module
