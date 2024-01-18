! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/point_assgn/fpAssgn011a2.f
! opt variations: -qnol

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
! %GROUP: fpAssgn011a2.f
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
!*  DATE                       : 02/17/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90/xlf95
!*
!*  DESCRIPTION                : pointer assignment (unlimited poly-pointer
!*                               assigned to an array section derived from
!*                               itself; test size() and associated(); use
!*                               intrinsic types and derived types)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type
end module

program fpAssgn011a2
use m
    type (base(20,4)), target :: b1(100)

    integer*4, target :: i1(-99:100)

    class (*), pointer :: x(:)

    b1 = (/(base(20,4)(i), i=1,100)/)

    i1 = (/(i, i=101,300)/)

    x => b1

    x => x(::2)

    if (size(x) /= 50) error stop 1_4

    if (.not. associated (x, b1(::2))) error stop 2_4

    x => i1

    x => x(1::2)

    if (size(x) /= 50) error stop 3_4

    if (.not. associated (x, i1(1::2))) error stop 4_4
end
