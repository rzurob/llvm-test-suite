!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/23/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Test nopass binding for
!                               extended type).
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains

        procedure, nopass :: str => stringRepBase
    end type

    contains

    character(i) function stringRepBase (b1, i)
        class(base), intent(in) :: b1
        integer, intent(in):: i

        stringRepBase = 'base: empty'
    end function
end module

module m1
use m, only : base

    type, extends(base) :: child (n)
        integer, len :: n

        character(n) :: name

        contains

        procedure, nopass :: str => stringRepChild
    end type

    contains

    character(i) function stringRepChild (b1, i)
        class(base), intent(in) :: b1
        integer, intent(in):: i

        stringRepChild = ''

        select type (b1)
            type is (base)
                stringRepChild = b1%str(b1, i)

            class is (child(*))
                stringRepChild = b1%name
        end select
    end function
end module

module m2
use m1
    type, extends(child) :: thirdGen (k)
        integer, kind :: k

        real(k) :: data(3)

        contains

        procedure, nopass :: str => string3rdGen
    end type

    abstract interface
        character(i) function generalFun (b1, i)
            use m
            class(base), intent(in) :: b1
            integer, intent(in):: i
        end function
    end interface

    procedure(generalFun) string3rdGen
end module

program dtpNopass002
use m2
    class(base), allocatable :: b1, b2(:,:), b3(:)
    character(:), allocatable :: s1

    allocate (b1)

    allocate (b2(2,2), source=child(20)('xlftest 101'))

    allocate (b3(10), source=thirdGen(15,4)('team effort', [1.2, 2.2, 3.2]))

    s1 = b1%str(b2(1,1), 10)

    if (s1%len /= 10) error stop 1_4
    if (s1 /= 'base: empt') error stop 2_4

    s1 = b2(:,2)%str(b2(1,2), 30)

    if (s1%len /= 30) error stop 3_4
    if (s1 /= 'xlftest 101') error stop 4_4

    s1 = b2%str(b1, 30)

    if ((s1%len /= 30) .or. (s1 /= 'base: empty')) error stop 5_4

    s1 = b3%str(b1, 20)

    if ((s1%len /= 20) .or. (s1 /= 'base: empty')) error stop 6_4

    s1 = b3(3)%str(b2(2,2), 25)

    if ((s1%len /= 25) .or. (s1 /= 'xlftest 101')) error stop 7_4

    s1 = b3%str(b3(7), 55)

    if ((s1%len /= 55) .or. (s1/= 'team effort      1.20      2.20      3.20'))&
        error stop 8_4

    s1 = b2%str(b3(5), 20)

    if ((s1%len /= 20) .or. (s1 /= 'team effort')) error stop 9_4
end

character(i) function string3rdGen (b1, i)
use m2, only: base, child, thirdGen
    implicit none
    class(base), intent(in) :: b1
    integer, intent(in):: i

    select type (b1)
        class is (base)
            string3rdGen = b1%str(b1, i)

        class is (thirdGen(*,4))
            write (string3rdGen, '(a, 3g10.3)') b1%name, b1%data

    end select
end function
