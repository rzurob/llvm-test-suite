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
!*  DATE                       : 02/24/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (private binding name
!                               overridden to be public)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure, private :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

end module

module m1
use m
    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'Child'
        print *, b%id, b%name
    end subroutine
end module


program ftpbnd504
use m1
    class (child), allocatable, target :: c1(:)
    class (base), pointer :: b(:)

    allocate (c1(10))

    c1%id = (/(i, i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i), i=1,10)/)

    b => c1(::2)

    select type (b)
        type is (child)
            if ((lbound(b,1) /= 1) .or. (ubound(b,1) /= 5)) error stop 1_4

            do i = 1, 5
                call b(i)%print
            end do
        class default
            error stop 2_4
    end select
end
