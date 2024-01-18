! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg517.f
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
! %GROUP: fArg517.f
! %VERIFY: 
! %STDIN:
! %STDOUT: fArg517.out
! %EXECARGS:
! %POSTCMD: spiff -r.000001 fArg517.out $TR_SRC/fArg517.vf  && rm -f fArg517.out
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
!*  DATE                       : 05/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (elemental procedure on
!                               two-dimensional array)
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

        procedure :: move => movePoint
    end type

    contains

    pure subroutine movePoint (p, dx, dy)
        class (point(4)), intent(inout) :: p
        real*4, intent(in) :: dx, dy

        p%x = p%x + dx
        p%y = p%y + dy
    end subroutine

    elemental subroutine move (p, dx, dy)
        real*4, intent(in) :: dx, dy
        !class (point), intent(inout) :: p
        type (point(4)), intent(inout) :: p

        call p%move (dx, dy)
    end subroutine
end module

program fArg517
use m
    type (point(4)) :: p1 (2, 3)

    p1 = reshape ((/point(4)(0.0, 0), point(4)(0,1), point(4)(1,0), point(4)(1,1), &
                    point(4)(1,2), point(4)(2,1)/), (/2,3/))

    call move (p1, dx=0.5, dy = 1.5)

    print *, p1
end
