! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr011.f
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
! %GROUP: fconstr011.f
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
!*  DESCRIPTION                : structure constructor (base type fully
!*                               initialized by default; only supply data for
!*                               extended type object)
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
    type base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        integer(k1)   :: id = 1
        real(k2)      :: value = 0.0
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20) (name = 'c1_m')
end module

program fconstr011
use m

    type (base(4,4)) :: b1
    type (child(4,4,1,20)) :: c1

    b1 = base(4,4)(2)

    c1 = child(4,4,1,20)(name = 'c1')

    if ((b1%id /= 2) .or. (b1%value /= 0.0)) error stop 1_4

    if ((c1%id /= 1) .or. (c1%value /= 0.0) .or. (c1%name /= 'c1')) error stop 2_4

    if ((c1_m%id /= 1) .or. (c1_m%value /= 0.0) .or. &
        (c1_m%name /= 'c1_m')) error stop 3_4
end
