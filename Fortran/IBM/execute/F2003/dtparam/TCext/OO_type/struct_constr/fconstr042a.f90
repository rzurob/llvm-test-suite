! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr042a.f
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
! %GROUP: fconstr042a.f
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
!*  DATE                       : 03/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (in DATA statement)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type (base(4)) :: b1_m(10)
    integer*4 :: i_m(10)

    DATA (i_m(i), i=1,5), (b1_m(i), i=2,6) /4*1, 10, 2*base(4)(id = 100), &
            3*base(4)(-1)/

    data b1_m(1), (b1_m(i), i=7,10) /base(4)(1), base(4)(7), base(4)(8), base(4)(id =9), base(4)(id=10)/

end module

program fconstr042a
use m
    type (base(4)) b1(6)

    data b1(1), (b1(i+1), i=1,3), (b1(i),i=5,6) /base(4)(id=1), base(4)(2), 1*base(4)(3), &
            0*base(4)(id=-1), 1*base(4)(id=4), base(4)(5), base(4)(6), 0*base(4)(-1)/

    !! verify b1
    do i=1, 6
        if (b1(i)%id /= i) error stop 1_4
    end do

    !! verify b1_m
    if (any (b1_m(2:3)%id /= 100)) error stop 2_4

    if (any (b1_m(4:6)%id /= -1)) error stop 3_4

    if (b1_m(1)%id /= 1) error stop 4_4

    do i = 2, 5
        if (b1_m(i+5)%id /= i+5) error stop 5_4
    end do

    !! verify i_m
    if (any (i_m(1:5) /= (/1, 1, 1, 1, 10/))) error stop 6_4

end
