module payload_test
    use payload_m, only: payload_t
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_payload

    type :: example_transferable_t
        private
        integer, allocatable :: int_data_(:)
        real, allocatable :: real_data_(:)
    contains
        private
        procedure, public :: int_data
        procedure, public :: real_data
        procedure, public :: serialize
    end type

    interface example_transferable_t
        module procedure construct
        module procedure deserialize
    end interface
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
                , it( &
                        "a derived type with allocatable components", &
                        check_allocatable_components_derived_type) &
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

    function check_allocatable_components_derived_type() result(result_)
        type(result_t) :: result_

        integer, parameter :: EXPECTED_INTS(*) = [1, 1, 2, 3, 5]
        real, parameter :: EXPECTED_REALS(*) = [3.14, 2.71, 6.02e23]
        integer :: i
        type(payload_t) :: payload
        type(example_transferable_t) :: retrieved
        type(example_transferable_t) :: to_send

        to_send = example_transferable_t(EXPECTED_INTS, EXPECTED_REALS)
        payload = payload_t(to_send%serialize())

        retrieved = example_transferable_t(payload%raw_payload())

        associate(retrieved_ints => retrieved%int_data())
            do i = 1, size(EXPECTED_INTS)
                result_ = result_.and.assert_equals(EXPECTED_INTS(i), retrieved_ints(i))
            end do
        end associate
        associate(retrieved_reals => retrieved%real_data())
            do i = 1, size(EXPECTED_REALS)
                result_ = result_.and.assert_equals(dble(EXPECTED_REALS(i)), dble(retrieved_reals(i)))
            end do
        end associate
    end function

    pure function construct(int_data, real_data) result(example_transferable)
        integer, intent(in) :: int_data(:)
        real, intent(in) :: real_data(:)
        type(example_transferable_t) :: example_transferable

        example_transferable%int_data_ = int_data
        example_transferable%real_data_ = real_data
    end function

    pure function deserialize(raw_data) result(example_transferable)
        integer, intent(in) :: raw_data(:)
        type(example_transferable_t) :: example_transferable

        integer, allocatable :: as_ints(:)

        as_ints = transfer(raw_data, as_ints)
        associate(int_size => as_ints(1))
            example_transferable%int_data_ = as_ints(2 : 1 + int_size)
            associate(real_size => as_ints(2+int_size))
                example_transferable%real_data_ = transfer( &
                        as_ints(3+int_size : 2+int_size+real_size), &
                        example_transferable%real_data_)
            end associate
        end associate
    end function

    pure function int_data(self)
        class(example_transferable_t), intent(in) :: self
        integer, allocatable :: int_data(:)

        if (allocated(self%int_data_)) int_data = self%int_data_
    end function

    pure function real_data(self)
        class(example_transferable_t), intent(in) :: self
        real, allocatable :: real_data(:)

        if (allocated(self%real_data_)) real_data = self%real_data_
    end function

    pure function serialize(self) result(raw_data)
        class(example_transferable_t), intent(in) :: self
        integer, allocatable :: raw_data(:)

        integer, allocatable :: int_portion(:)
        integer, allocatable :: real_portion(:)

        if (allocated(self%int_data_)) then
            int_portion = [size(self%int_data_), self%int_data_]
        else
            int_portion = [0]
        end if

        if (allocated(self%real_data_)) then
            real_portion = [size(self%real_data_), transfer(self%real_data_, real_portion)]
        else
            real_portion = [0]
        end if

        raw_data = transfer([int_portion, real_portion], raw_data)
    end function
end module
