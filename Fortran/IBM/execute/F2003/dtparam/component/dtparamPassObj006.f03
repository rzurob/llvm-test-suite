! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.3: passed-object)
!                               Case: Extend dtparamPassObj003 by extending the
!                               base types and corresponding bindings.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base1 (k)
        integer, kind :: k

        real(k) :: data

        contains

        procedure, pass(b1) :: calc4 => addAll4
        procedure, pass(b1) :: calc8 => addAll8
    end type

    type base2 (n)
        integer, len :: n

        real :: data(n)

        contains

        procedure, pass(b2) :: calc4 => addAll4
        procedure, pass(b2) :: calc8 => addAll8
    end type

    type, extends(base1) :: child1
        real(k) :: weighedSum

        contains

        procedure, pass(b1) :: calc4 => addChild1_4
        procedure, pass(b1) :: calc8 => addChild1_8
    end type

    type, extends(base2) :: child2
        contains

        procedure, pass(b2) :: calc4 => addChild2_4
        procedure, pass(b2) :: calc8 => addChild2_8
    end type

    contains

    subroutine addAll4 (b1, b2)
        class (base1(4)), intent(inout) :: b1
        class (base2(*)), intent(in) :: b2

        b1%data = sum (b2%data)
    end subroutine

    subroutine addAll8 (b1, b2)
        class (base1(8)), intent(inout) :: b1
        class (base2(*)), intent(in) :: b2

        b1%data = product(real(b2%data, 8))
    end subroutine

    subroutine addChild1_4 (b1, b2)
        class(child1(4)), intent(inout) :: b1
        class(base2(*)), intent(in) :: b2

        call b1%base1%calc4(b2)

        b1%weighedSum = sum(b2%data, mask=b2%data>0)
    end subroutine

    subroutine addChild1_8 (b1, b2)
        class(child1(8)), intent(inout) :: b1
        class(base2(*)), intent(in) :: b2

        call b1%base1%calc8(b2)

        b1%weighedSum = product(real(b2%data, 8), mask=b2%data>0)
    end subroutine

    subroutine addChild2_4 (b1, b2)
        class(base1(4)), intent(inout) :: b1
        class(child2(*)), intent(in) :: b2

        call b1%calc4(b2)
    end subroutine

    subroutine addChild2_8 (b1, b2)
        class(base1(8)), intent(inout) :: b1
        class(child2(*)), intent(in) :: b2

        call b1%calc8(b2)
    end subroutine
end module

program dtparamPassObj006
use m
    class (base1(4)), allocatable :: b1_1(:)
    class (base2(:)), allocatable :: b2_1, b2_2
    class (base1(8)), pointer :: b1_2(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (b1_1(4))
    allocate (base2(10) :: b2_1)
    allocate (child2(20) :: b2_2)
    allocate (child1(8) :: b1_2(10))

    b2_1%data = (/(i*1.0, i=-4,5)/)
    b2_2%data = (/(i*1.0, i=-7,12)/)

    !! set up b1_1(1:2) through calls to calc4 via b2_1
    call b1_1(1)%calc4(b2_1)
    call b2_1%calc4(b1_1(2))


    !! set up b1_1(3:4) through calls to calc4 via b2_2
    call b1_1(3)%calc4(b2_2)
    call b2_2%calc4(b1_1(4))


    !! set up b1_2(6:7) through calls to calc8 via b2_2
    call b1_2(6)%calc8 (b2_2)
    call b2_2%calc8 (b1_2(7))


    !! set up b1_2(3:4) through calls to calc8 via b2_1
    call b1_2(3)%calc8 (b2_1)
    call b2_1%calc8 (b1_2(4))

    !! verify the results
    write (*, '(4f10.2)') b1_1%data

    write (*, '(4f10.2)') abs(b1_2(3:4)%data), abs(b1_2(6:7)%data)

    select type (b1_2)
        type is (child1(8))
            write (*, '(3g10.2)') b1_2(3)%weighedSum, b1_2(6:7)%weighedSum

        class default
            stop 10
    end select
end
