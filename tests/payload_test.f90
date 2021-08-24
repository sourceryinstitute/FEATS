module payload_test
    use payload_m, only: payload_t
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_payload
contains
    function test_payload() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "a payload can store and retrieve", &
                [ it("a string", check_string) &
                , it("a scalar integer", check_scalar_integer) &
                , it("a scalar real", check_scalar_real) &
                , it("an integer array", check_integer_array) &
                , it("a simple derived type", check_simple_derived_type) &
                ! , it( &
                !         "a derived type with an allocatable component", &
                !         check_allocatable_components_derived_type) &
                ])
    end function

    function check_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXPECTED = "Hello, World!"
        type(payload_t) :: payload

        payload = payload_t(EXPECTED)

        result_ = assert_equals(EXPECTED, payload%string_payload())
    end function

    function check_scalar_integer() result(result_)
        type(result_t) :: result_

        integer, parameter :: EXPECTED = 42
        type(payload_t) :: payload

        payload = payload_t(transfer(EXPECTED, payload%raw_payload()))

        result_ = assert_equals( &
                EXPECTED, &
                transfer(payload%raw_payload(), EXPECTED))
    end function

    function check_scalar_real() result(result_)
        type(result_t) :: result_

        double precision, parameter :: EXPECTED = 3.14d0
        type(payload_t) :: payload

        payload = payload_t(transfer(EXPECTED, payload%raw_payload()))

        result_ = assert_equals( &
                EXPECTED, &
                transfer(payload%raw_payload(), EXPECTED))
    end function

    function check_integer_array() result(result_)
        type(result_t) :: result_

        integer, parameter :: EXPECTED(*) = [1, 1, 2, 3, 5]
        integer, allocatable :: expected_(:)
        integer :: i
        type(payload_t) :: payload
        integer, allocatable :: retrieved(:)

        expected_ = EXPECTED
        payload = payload_t(transfer(expected_, payload%raw_payload()))

        retrieved = transfer(payload%raw_payload(), retrieved)

        do i = 1, size(EXPECTED)
            result_ = result_.and.assert_equals(EXPECTED(i), retrieved(i))
        end do
    end function

    function check_simple_derived_type() result(result_)
        type(result_t) :: result_

        type :: my_type
            integer :: x
            double precision :: y
        end type

        type(my_type), parameter :: EXPECTED = my_type(42, 3.14d0)
        type(payload_t) :: payload
        type(my_type) :: retrieved

        payload = payload_t(transfer(EXPECTED, payload%raw_payload()))

        retrieved = transfer(payload%raw_payload(), retrieved)

        result_ = &
                assert_equals(EXPECTED%x, retrieved%x) &
                .and. assert_equals(EXPECTED%y, retrieved%y)
    end function

    ! function check_allocatable_components_derived_type() result(result_)
    !     type(result_t) :: result_
    !
    !     type :: my_type
    !         integer, allocatable :: vals(:)
    !     end type
    !
    !     integer, parameter :: EXPECTED(*) = [1, 1, 2, 3, 5]
    !     integer :: i
    !     type(payload_t) :: payload
    !     type(my_type) :: retrieved
    !     type(my_type) :: to_send
    !
    !     to_send = my_type(EXPECTED)
    !     payload = payload_t(transfer(to_send, payload%raw_payload()))
    !
    !     retrieved = transfer(payload%raw_payload(), retrieved)
    !
    !     do i = 1, size(EXPECTED)
    !         result_ = result_.and.assert_equals(EXPECTED(i), retrieved%vals(i))
    !     end do
    ! end function
end module
