! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd525.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 06/11/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : type-bound procedure (private keyword on
!                               type-bound part ONLY affects type-bound part)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)         x, y

        contains

        private     !<-- this keyword only affects type-bound part of this type
        procedure :: notToUse => hidden

        procedure, public :: print => printPoint
    end type

    type, extends(point) :: point3D    ! (20,4)
        real(k1) :: z = 0.5

        contains

        procedure :: print => printPoint3D
    end type

    private hidden

    contains

    subroutine hidden (p)
        class(point(*,4)), intent(in) :: p
    end subroutine

    subroutine printPoint (p, fmt)
        class(point(*,4)), intent(in) :: p
        character(*), intent(in) :: fmt

        write (*, fmt) p%x, p%y
    end subroutine

    subroutine printPoint3D (p, fmt)
        class(point3D(*,4)), intent(in) :: p
        character(*), intent(in) :: fmt

        write (*, fmt) p%x, p%y, p%z
    end subroutine
end module

program ftpbnd525
use m
    class (point(:,4)), allocatable :: p1(:)
    type (point(20,4)) :: p2 = point(20,4)(1.0, 3.0)

    allocate(point3D(20,4):: p1(10))

    do i = 1, 10
        p1(i)%x = 1.0
        p1(i)%y = 3.0
    end do

    call p2%print("('x = ',f10.2,'; y = ', f10.2))")

    do i = 1, 10
        call p1(i)%print (fmt = '(3f10.3)')
    end do
end
