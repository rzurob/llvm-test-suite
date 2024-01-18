! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr004.f
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
    type point(k1)    ! (4)
        integer, kind          :: k1
        real(k1), dimension(2) :: coord = (/0.0, 0.0/)
    end type

    type line(k2,n1)    ! (4,20)
        integer, kind                 :: k2
        integer, len                  :: n1
        type(point(k2)), dimension(2) :: x! = (/ point(k2)((/1.0, 2.0/)), &
!            point(k2)((/4.0, 6.0/))/)
    end type

    contains

    function initial_points () result(res)
        type(point(4)) res(2)

        res = [point(4)([1.0, 2.0]), point(4)([4.0,6.0])]
    end function
end module

program fconstr004
use m

    implicit none
    type(point(4)), parameter :: p = point(4) ((/0.0, 1.0/))
    type(line(4,20)) :: lineSeq

    lineSeq%x = initial_points()

    print*, lineSeq%x(1)
    print*, lineSeq%x(2)

    print *, p

    if (lineSeq%x(1)%coord(1) /= 1.0) error stop 1_4
    if (lineSeq%x(1)%coord(2) /= 2.0) error stop 2_4

    if (lineSeq%x(2)%coord(1) /= 4.0) error stop 3_4
    if (lineSeq%x(2)%coord(2) /= 6.0) error stop 4_4
end
