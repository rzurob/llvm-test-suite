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
!*  DATE                       : 04/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (array element does not
!                               have POINTER/ALLOCATABLE attribute)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine createBase (b, id, name)
        class (base), allocatable, intent(out) :: b
        integer*4, intent(in) :: id
        character(*), optional, intent(in) :: name

        if (present (name)) then
            allocate (b, source=child(id, name))
        else
            allocate (b)
            b%id = id
        end if
    end subroutine

    subroutine printBasePtr (b)
        class (base), pointer :: b

        if (associated (b)) call b%print
    end subroutine
end module

program fArg005d
use m
    class (base), allocatable :: b1(:)

    class (base), pointer :: b2(:,:)

    allocate (b1(2))
    allocate (b2 (10,10))

    call createBase (b1(2), 1, 'fail')  !<-- element does NOT have allocatable attr.

    call printBasePtr (b2(2, 3))        !<-- element does NOT have pointer attr.
end
