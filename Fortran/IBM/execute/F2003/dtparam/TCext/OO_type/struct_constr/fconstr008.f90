! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr008.f
! with manual adjustment to original (moving def of base before child)
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
! %GROUP: fconstr008.f
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
!*  DESCRIPTION                : structure constructor (no more component in
!*                               extending type)
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
    type base(k1,k2,n1)    ! (4,1,20)
        integer, kind             :: k1,k2
        integer, len              :: n1
        integer(k1)               :: id
        character(kind=k2,len=n1) :: name
    end type

    type, extends(base) :: child(k3,n2)    ! (4,1,20,4,20)
        integer, kind :: k3
        integer, len  :: n2
    end type

    type(base(4,1,20)) :: b1_m = base(4,1,20)(10, 'module data b1_m')
    type(child(4,1,20,4,20)) :: c1_m = child(4,1,20,4,20)(20, 'module data c1_m')
end module

program fconstr008
use m

    type (base(4,1,20)) :: b1 = base(4,1,20)(0, 'test data b1')
    type (child(4,1,20,4,20)) :: c1 = child(4,1,20,4,20)(1, 'test data c1')

    if (b1_m%id /= 10) error stop 1_4
    if (b1_m%name /= 'module data b1_m') error stop 2_4

    if (c1_m%id /= 20) error stop 3_4
    if (c1_m%name /= 'module data c1_m') error stop 4_4

    if (b1%id /= 0) error stop 5_4
    if (b1%name /= 'test data b1') error stop 6_4

    if (c1%id /= 1) error stop 7_4
    if (c1%name /= 'test data c1') error stop 8_4
end
