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
! %GROUP: fconstr004.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
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
!*  DATE                       : Nov. 12, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (default initialization
!*                               for the component that is of derived type)
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
    type point
        real*4, dimension(2) :: coord = (/0.0, 0.0/)
    end type

    type line
        type (point), dimension(2) :: x = (/ point((/1.0, 2.0/)), &
            point((/4.0, 6.0/))/)
    end type

end module

program fconstr004
use m

    type(point), parameter :: p = point ((/0.0, 1.0/))
    type(line) :: lineSeq

    print*, lineSeq%x(1)
    print*, lineSeq%x(2)

    print *, p

    if (lineSeq%x(1)%coord(1) /= 1.0) error stop 1_4
    if (lineSeq%x(1)%coord(2) /= 2.0) error stop 2_4

    if (lineSeq%x(2)%coord(1) /= 4.0) error stop 3_4
    if (lineSeq%x(2)%coord(2) /= 6.0) error stop 4_4
end
