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
!*  DATE                       : 05/19/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (VALUE attribute and
!                               TAGET)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains
    
    subroutine t1 (i1)
        class(*), target :: i1(:)
        class(*), pointer :: i2(:), i3(:)

        i2 => i1(::3)

        call test1 (i1(::3))

        if (.not. associated (i3, i2)) error stop 1_4

        contains

        subroutine test1 (i)
            class(*), target :: i(:)

            if (.not. associated (i2, i)) error stop 2_4

            i3 => i
        end subroutine
    end subroutine
end module

program fArg013a7_1
use m
    class(*), pointer :: x1(:)
    class(*), allocatable, target :: x2(:,:)

    allocate (character(20) :: x1(10))

    allocate (real(8) :: x2(10,200))

    call t1(x1)

    call t1 (x2(3,:))
end
