! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr011a.f
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
! %GROUP: fconstr011a.f
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
!*  DESCRIPTION                : structure constructor (extended type has all
!*                               additional components default initialized)
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
        integer(k1)   :: id
        real(k2)      :: value
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name = ''
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet = .false.
    end type

    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20) (10, 1.0)
    type (thirdGeneration(4,4,1,20,1)), save :: t1_m = thirdGeneration(4,4,1,20,1) (id = 20, value=1.0)
end module

program fconstr011a
use m

    type (base(4,4)) :: b1
    type (child(4,4,1,20)) :: c1 = child(4,4,1,20)(2, 2.0)
    type (thirdGeneration(4,4,1,20,1)) :: t1

    c1 = child(4,4,1,20)(2, 2.0)
    t1 = thirdGeneration(4,4,1,20,1) (3, 3.0, 't1')

    if ((c1%id /= 2) .or. (c1%value /= 2.0) .or. (c1%name /= '')) error stop 1_4

    if ((t1%id /= 3) .or. (t1%value /= 3.0) .or. (t1%name /= 't1') .or. &
        t1%isSet) error stop 2_4

    if ((c1_m%id /= 10) .or. (c1_m%value /= 1.0) .or. &
        (c1_m%name /= '')) error stop 3_4

    if ((t1_m%id /= 20) .or. (t1_m%value /= 1.0) .or. &
        (t1_m%name /= '') .or. t1_m%isSet) error stop 4_4
end
