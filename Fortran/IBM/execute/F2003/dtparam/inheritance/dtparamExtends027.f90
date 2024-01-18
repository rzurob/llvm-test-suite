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
!*  DATE                       : 12/15/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: type parameter is inherited, and parent
!                               type name is make accessible via use only stmt.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, l)
        integer, kind :: k = 4
        integer, len  :: l = 10

        private

        integer(k) :: id = -10
        character(l) :: name = 'default'
    end type

    type, extends(base) :: child
    end type

    interface updateChild
        module procedure updateChild4
        module procedure updateChild8
    end interface

    interface printChild
        module procedure printChild4
        module procedure printChild8
    end interface

    contains

    subroutine updateChild4 (c, id, name)
        type (child(4, *)), intent(inout) :: c
        integer(4), intent(in) :: id
        character(len=*), intent(in) :: name

        c%id = id
        c%name = name
    end subroutine

    subroutine updateChild8 (c, id, name)
        type (child(8, l=*)), intent(inout) :: c
        integer(8), intent(in) :: id
        character(len=*), intent(in) :: name

        c%id = id
        c%name = name
    end subroutine

    subroutine printChild4 (c)
        type (child(k=4, l=*)), intent(in) :: c

        print *, '4: ', c%id, c%name
    end subroutine

    subroutine printChild8 (c)
        type (child(8, *)), intent(in) :: c

        print *, '8: ', c%id, c%name
    end subroutine
end module

module m1
use m, only : child
    type, extends(child) :: gen3
        logical(k) :: flag(l) = .true.
    end type
end module

program dtparamExtends027
use m, only: updateChild, printChild
use m1
    type(gen3(4, 16)) g1(2)
    type (gen3(8, 20)), parameter :: g2 = gen3(8, 20)&
                (flag=(/(.true., .false., i=1, 10)/))

    call updateChild(g1(2)%child, 10, 'g1 2nd element')
    g1(1)%flag = (/(.false., .true., .true., .false., i = 1, 4)/)

    call printChild (g1(1)%child)
    print *, g1(1)%flag

    call printChild (g1(2)%child)
    print *, g1(2)%flag

    call printChild (g2%child)
    print *, g2%flag
end
