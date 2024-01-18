! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn015.f
! opt variations: -ql

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn015.f
! %VERIFY: 
! %STDIN:
! %STDOUT: fpAssgn015.out
! %EXECARGS:
! %POSTCMD: spiff -r1.e-7 fpAssgn015.out $TR_SRC/fpAssgn015.vf
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (one dimensional array
!                               pointer assigned to array section from
!                               2-dimensional array)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: x, y

        contains

        procedure :: print => printPoint
    end type

    type, extends (point) :: colorPoint(k2)    ! (4,2)
        integer, kind :: k2
        integer(k2)   :: color

        contains

        procedure :: print => printColorPoint
    end type

    class (point(4)), pointer :: p(:)

    contains

    subroutine printPoint (p)
        class (point(4)), intent(in) :: p

        print *, p%x, p%y
    end subroutine

    subroutine printColorPoint (p)
        class (colorPoint(4,2)), intent(in) :: p
        print *, p%x, p%y, p%color
    end subroutine
end module

use m
    type (colorPoint(4,2)), target :: cp(2,3)

    cp = reshape ((/ colorPoint(4,2)(0,0,1), colorPoint(4,2)(1,0,1),colorPoint(4,2)(0,1,1), &
                   colorPoint(4,2)(1,1,1), colorPoint(4,2)(1,1,2), colorPoint(4,2)(0,0,2)/), &
                  (/2,3/))

    p => cp(1,:)

    if (size (p) /= 3) error stop 1_4

    if (lbound (p, 1) /= 1) error stop 2_4

    if (.not. associated (p, cp(1,:))) error stop 3_4

    call p(1)%print     ! 0 0 1
    call p(2)%print     ! 0 1 1
    call p(3)%print     ! 1 1 2


    p => cp (:, 2)

    if (size (p) /= 2) error stop 4_4

    call p(1)%print     ! 0 1 1
    call p(2)%print     ! 1 1 1
end
