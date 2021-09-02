module compile_or_link_m
  !! Define a tasks that the scheduler assigns
  !! that a compute image executes.
  use data_location_map_m, only: data_location_map_t
  use payload_m, only: payload_t
  use task_m, only : task_t
  implicit none

  private
  public :: compile_or_link_t

  type, extends(task_t) :: compile_or_link_t
    !! encapsulate task work
    private
  contains
    procedure :: execute
  end type

  interface

    module function execute(self, input_locations, task_number, mailbox) result(output)
      !! complete the assigned task
      implicit none
      class(compile_or_link_t), intent(in) :: self
      type(data_location_map_t), intent(in) :: input_locations
      integer, intent(in) :: task_number
      type(payload_t), intent(in) :: mailbox(:)[*]
      type(payload_t) :: output
    end function


  end interface

end module compile_or_link_m
