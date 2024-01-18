!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.3: passed-object)
!                               Case: Test the overriding binding with the
!                               length type parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n) = 0.0

        contains

        procedure :: update => updateBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        logical :: mask (n) = .true.

        contains

        procedure :: update => updateChild
        procedure :: print => printChild
    end type

    contains

    subroutine updateBase (b, r1)
        class(base(*)), intent(inout) :: b
        real, intent(in) :: r1(:)

        if (size(r1) < b%n) then
            print *, 'not enough data as input for r1'
            stop 10
        else
            b%data = r1(:b%n)
        end if
    end subroutine

    subroutine updateChild (b, r1)
        class(child(*)), intent(inout) :: b
        real, intent(in) :: r1(:)

        call b%base%update(r1)

        !! set up the mask for the positive values only
        b%mask = b%data > 0.0
    end subroutine

    subroutine printBase (b)
        class(base(*)), intent(in) :: b

        write (*, '(5f10.2)') b%data
    end subroutine

    subroutine printChild (b)
        class(child(*)), intent(in) :: b

        k = 1
        do i = 1, b%n
            if (.not. b%mask(i)) cycle

            if (k == 5) then
                write (*, '(f10.2)') b%data(i)

                k = 1
            else
                write (*, '(f10.2)', advance='no') b%data(i)

                k = k + 1
            end if
        end do

        print *, ''
    end subroutine
end module

program dtparamPassObj002
use m
    class(base(30)), pointer :: b1
    class(base(:)), allocatable :: b2

    real, allocatable :: r1(:)

    allocate (b1)
    allocate (child(50) :: b2)

    allocate (r1(100), source=(/(i*1.0, i=-49, 50)/))

    call b1%update(r1(40:))

    call b2%update((/r1(1:10), r1(55:60), r1(53:40:-2), r1(70:), r1(10:25)/))

    call b1%print

    print *, 'test 2'

    call b2%print
end
