module data_location_map_test
    use data_location_map_m, only: data_location_map_t
    use vegetables, only: result_t, test_item_t, assert_equals, given, then_, when

    implicit none
    private
    public :: test_data_location_map
contains
    function test_data_location_map() result(tests)
        type(test_item_t) :: tests

        tests = given( &
                "a data_location_map_t", &
                [ when( &
                        "it is queried for the location a task was executed", &
                        [ then_( &
                                "it returns the image corresponding the task", &
                                check_location_of) &
                        ]) &
                ])
    end function

    function check_location_of() result(result_)
        type(result_t) :: result_

        type(data_location_map_t) :: map

        map = data_location_map_t([1, 3, 5], [2, 4, 6])

        result_ = &
                assert_equals(2, map%location_of(1)) &
                .and. assert_equals(4, map%location_of(3)) &
                .and. assert_equals(6, map%location_of(5))
    end function
end module
