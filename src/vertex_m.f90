module vertex_m
  !! summary: Represent one node in a directed acyclic graph.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020-2021, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
    use task_m, only: task_t
    implicit none

    private
    public :: vertex_t

    type vertex_t
      !! Encapsulate a node in a graph comprised of vertices connected by dependencies (edges)
      integer, allocatable :: edges(:)
      class(task_t), allocatable :: task
    end type

    interface vertex_t
      module procedure construct
    end interface
contains
    function construct(edges, task) result(vertex)
      integer, intent(in) :: edges(:)
      class(task_t), intent(in) :: task
      type(vertex_t) :: vertex

      vertex%edges = edges
      vertex%task = task
    end function
end module vertex_m
