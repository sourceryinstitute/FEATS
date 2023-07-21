module feats_application_generator_m
  use application_m, only: application_t
  use dag_m, only : dag_t
  use payload_m, only: payload_t, empty_payload
  use task_m, only: task_t
  use task_item_m, only: task_item_t
  use vertex_m, only : vertex_t

  implicit none
  private
  public :: generate_application

  type, extends(task_t) :: compile_task_t
      character(len=:), allocatable :: to_compile
  contains
      procedure :: execute => compile_task_execute
  end type
contains
  function generate_application() result(application)
    type(application_t) :: application
      block
        character(len=*),           parameter :: external_ = 'shape=square,fillcolor="green",style=filled'
        character(len=*),           parameter :: root      = 'shape=circle,fillcolor="white",style=filled'
        character(len=*),           parameter :: branch    = 'shape=square,fillcolor="SlateGray1",style=filled'
        character(len=len(branch)), parameter :: leaf      = 'shape=circle,fillcolor="cornsilk",style=filled'
        type(dag_t) feats
        type(task_item_t), allocatable :: tasks(:)

        feats = dag_t(&
            [ vertex_t([integer::], "assert_m", external_) & ! 1
            , vertex_t([integer::], "dag_m", external_) & ! 2
            , vertex_t([2, 13], "application_m", branch) & ! 3
            , vertex_t([1, 3], "application_s", root) & ! 4
            , vertex_t([integer::], "feats_result_map_m", leaf) & ! 5
            , vertex_t([11, 15], "final_task_m", branch) & ! 6
            , vertex_t([6], "final_task_s", root) & ! 7
            , vertex_t([3, 5, 11], "image_m", branch) & ! 8
            , vertex_t([2, 6, 8, 10, 13], "image_s", root) & ! 9
            , vertex_t([11], "mailbox_m", branch) & ! 10
            , vertex_t([integer::], "payload_m", leaf) & ! 11
            , vertex_t([11], "payload_s", root) & ! 12
            , vertex_t([11, 15], "task_item_m", branch) & ! 13
            , vertex_t([13], "task_item_s", root) & ! 14
            , vertex_t([11], "task_m", branch) & ! 15
            , vertex_t([15], "task_s", root) & ! 16
            ])
        tasks = &
        [ task_item_t(compile_task_t("assert_m")) &
        , task_item_t(compile_task_t("dag_m")) &
        , task_item_t(compile_task_t("application_m")) &
        , task_item_t(compile_task_t("application_s")) &
        , task_item_t(compile_task_t("feats_result_map_m")) &
        , task_item_t(compile_task_t("final_task_m")) &
        , task_item_t(compile_task_t("final_task_s")) &
        , task_item_t(compile_task_t("image_m")) &
        , task_item_t(compile_task_t("image_s")) &
        , task_item_t(compile_task_t("mailbox_m")) &
        , task_item_t(compile_task_t("payload_m")) &
        , task_item_t(compile_task_t("payload_s")) &
        , task_item_t(compile_task_t("task_item_m")) &
        , task_item_t(compile_task_t("task_item_s")) &
        , task_item_t(compile_task_t("task_m")) &
        , task_item_t(compile_task_t("task_s")) &
        ]
        application = application_t(feats, tasks)
      end block
  end function

  function compile_task_execute(self, arguments) result(output)
    class(compile_task_t), intent(in) :: self
    type(payload_t), intent(in) :: arguments(:)
    type(payload_t) :: output

    real :: rand
    character(len=10) :: image_string

    write(image_string, "(I0)") this_image()
    print *, "Compiling: " // self%to_compile &
        // " on image number: " // trim(image_string)
    call random_number(rand)
    call sleep(int(rand * 10))
    print *, "Finished Compiling: " // self%to_compile

    output = empty_payload()
  end function
end module
