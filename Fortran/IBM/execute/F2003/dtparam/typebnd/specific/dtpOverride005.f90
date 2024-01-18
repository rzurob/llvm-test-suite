!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/19/2009
!*
!*  DESCRIPTION                : dtparam: test a pure type bound procedure is
!used in a specification expression.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type realType (p, r)
        integer, len :: p, r

        contains

        procedure :: precision => getPrecision
    end type

    contains

    integer pure function getPrecision (rT)
        class(realType(*,*)), intent(in) :: rT

        real, parameter :: r4 = 0.0
        double precision, parameter :: d8 = 0.0
        real(16), parameter :: q16 = 0.0

        select case (selected_real_kind(rT%p, rT%r))
            case (r4%kind)
                getPrecision = precision (r4)

            case (d8%kind)
                getPrecision = precision (d8)

            case (q16%kind)
                getPrecision = precision (q16)

            case default
                getPrecision = 0
        end select
    end function
end module


module m1
use m
    type, extends(realType) :: extendedRealType (radix)
        integer, len :: radix = 2

        contains

        procedure :: precision => getPrecisionExtended
    end type

    contains

    pure integer function getPrecisionExtended (rT)
        class(extendedRealType(*,*,*)), intent(in) :: rT

        if (rT%radix == 2) then
            getPrecisionExtended = rT%realType%precision()
        else if (rT%radix == 10) then
            select case (rT%p)
                case (1:7)
                    getPrecisionExtended = 7

                case (8:16)
                    getPrecisionExtended = 16

                case (17:34)
                    getPrecisionExtended = 34

                case default
                    getPrecisionExtended = 0
            end select
        else
            getPrecisionExtended = -1
        end if
    end function
end module

program dtpOverride005
use m1
    type(realType(p=10, r=80)) d1
    type(extendedRealType(14,100,radix=10)) dfp64
    class(realType(:,:)), allocatable :: array(:)
    class(realType(:,:)), pointer :: arrayPtr(:)

    integer testArray (100)

    allocate (extendedRealType(24,100,10) :: array(10))

    if (d1%precision() /= precision(1.0d0))  error stop 1_4
    if (dfp64%precision() /= 16) error stop 2_4
    if (array(10)%precision() /= 34) error stop 3_4

    ! test 1
    call test1 (d1, testArray, 15)
    call test1 (dfp64, testArray, dfp64%precision())
    call test1 (array(1), testArray, array(2)%precision())

    deallocate (array)

    ! test 2
    allocate (realType(6,20) :: array(20))

    call test2 (d1, d1%precision())
    call test2 (dfp64, dfp64%precision())
    call test2 (array(5), array(1)%precision())

    !test3
    testArray(:15) = test3(d1)
    testArray(16:31) = test3(dfp64)

    allocate (extendedRealType(30, 200, 10) :: arrayPtr(20))

    testArray(32:65) = test3 (arrayPtr(10))

    if (any (testArray(:65) /= [(16-i, i=1,15), (17-i,i=1,16), (35-i,i=1,34)]))  then
        error stop 10_4
    end if

    !test4

    if (test4(d1)    /= '000000000000000') error stop 12_4
    if (test4(dfp64) /= '0000000000000000') error stop 13_4
    if (test4(array(10)) /= '000000') error stop 14_4
    if (test4(arrayPtr(1)) /= '0000000000000000000000000000000000') error stop 15_4

    contains

    subroutine test1 (x1, d1, testSize)
        class(realType(*,*)), intent(in) :: x1
        integer, intent(in) :: d1(x1%precision())
        integer, intent(in) :: testSize

        if (size(d1) /= testSize) stop 100
    end subroutine

    subroutine test2 (x1, testSize)
        class(realType(*,*)), intent(in) :: x1
        integer, intent(in) :: testSize

        real localArray(x1%precision())

        if (size(localArray) /= testSize) stop 200
    end subroutine

    function test3(x1)
        class(realType(*,*)), intent(in) :: x1

        integer test3(x1%precision())

        integer i

        do i = 1, size(test3)
            test3(i) = size(test3) + 1 -i
        end do
    end function

    function test4(x1)
        class (realType(*,*)), intent(in) :: x1

        character(x1%precision()) test4

        integer i

        do i = 1, len(test4)
            test4(i:i) = '0'
        end do
    end function
end
