! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd520d.f
! opt variations: -qnol

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
!*  DATE                       : 04/11/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (diagnostic testing on
!                               protected attributes)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = 1

        contains

        procedure :: print => printBase
        procedure :: addID => addID2Base
    end type

    type (base(20,4)), protected, target :: b1_m = base(20,4) (10)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addID2Base (b, i)
        class (base(*,4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine
end module

program ftpbnd520d
use m
    class (*), pointer :: x1

    associate (x => b1_m)
        call x%addID (9)    !<-- illegal

        x1 => x             !<-- illegal

        x%id = 100          !<-- illegal
    end associate

    call b1_m%print
end
