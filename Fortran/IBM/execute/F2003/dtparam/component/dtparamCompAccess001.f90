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
!*  DATE                       : 01/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Derived type parameter and private access
!                               for component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k), private :: ids(n)

        contains

        generic :: update => update4, update8
        procedure :: update4 => updateBase4
        procedure :: update8 => updateBase8

        generic :: print => print4, print8
        procedure :: print4 => printBase4
        procedure :: print8 => printBase8
    end type

    contains

    subroutine updateBase4 (b, i1)
        class (base(4,*)), intent(out) :: b
        integer(4), intent(in) :: i1(b%n)

        b%ids = i1
    end subroutine

    subroutine updateBase8 (b, i1)
        class(base(8,*)), intent(out) :: b
        integer(8), intent(in) :: i1(b%n)

        b%ids = i1
    end subroutine

    subroutine printBase4 (b)
        class(base(4, *)), intent(in) :: b

        print *, b%ids
    end subroutine

    subroutine printBase8 (b)
        class(base(8, *)), intent(in) :: b

        print *, b%ids
    end subroutine
end module

program dtparamCompAccess001
use m
    class(base(4, :)), allocatable :: b1
    class (base(8, :)), pointer :: b2(:)

    integer(8) i2(100)

    allocate (base(4, 10) :: b1)
    allocate (base(8, 5) :: b2(10))

    call b1%update((/(i, i=1, 10)/))

    i2 = (/(2_8**33*i, i = 1, 100)/)

    do i = 1, 10
        call b2(i)%update(i2((i-1)*10+1:))
    end do

    !! verify
    call b1%print

    do i = 1, 10
        call b2(i)%print
    end do
end
