! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd527.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 11/08/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (use the external procedure
!                               for the deferred binding)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module modA
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure(p), deferred :: p1
    end type

    interface               !<-- better, abstract interface
        subroutine p (b)
        import
            class(base(4,*)), intent(in) :: b
        end subroutine
    end interface
end module

module modB
use modA
    type, extends(base) :: child    ! (4,20)
        real(k1) data

        contains

        procedure :: p1
    end type

    interface
        subroutine p1 (b)
        import
            class(child(4,*)), intent(in) :: b
        end subroutine
    end interface
end module

program ftpbnd527
use modB
    integer, parameter :: sizeB = 5000
    class(base(4,:)), allocatable :: b1(:)

    allocate (b1(sizeB), source=(/(child(4,20)(i), i=1,sizeB)/))

    do i = 1, sizeB, sizeB/10
        call b1(i)%p1
    end do
end

subroutine p1 (b)
use modB, only: child
    class(child(4,*)), intent(in) :: b

    write (*, '(f12.2)') b%data
end subroutine
