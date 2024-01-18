! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr009.f
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
! %GROUP: fconstr009.f
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
!*  DESCRIPTION                : structure constructor (all components default
!*                               initialized, omitting them)
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
        character(kind=k3,len=n1) :: name = 'test'
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet = .false.
    end type

    type (base(4,4)), save :: b1_m = base(4,4)()
    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20)()
    type (thirdGeneration(4,4,1,20,1)), save :: t1_m = thirdGeneration(4,4,1,20,1)()


end module

program fconstr009
use m

    type (base(4,4)) :: b1
    type (child(4,4,1,20)) :: c1
    type (thirdGeneration(4,4,1,20,1)) :: t1

    type (base(4,4)) :: b2 = base(4,4)()
    type (child(4,4,1,20)) :: c2 = child(4,4,1,20)()
    type (thirdGeneration(4,4,1,20,1)) :: t2 = thirdGeneration(4,4,1,20,1)()

    b1 = base(4,4)()
    c1 = child(4,4,1,20)()
    t1 = thirdGeneration(4,4,1,20,1)()

    if ( (b1%id /= 1) .or. (b1%value /= 0.0)) error stop 1_4

    if ( (c1%id /= 1) .or. (c1%value /= 0.0) .or. &
         (c1%name /= 'test')) error stop 2_4

    if ( (t1%id /= 1) .or. (t1%value /= 0.0) .or. &
         (t1%name /= 'test') .or. t1%isSet) error stop 3_4

    if ( (b2%id /= 1) .or. (b2%value /= 0.0)) error stop 4_4

    if ( (c2%id /= 1) .or. (c2%value /= 0.0) .or. &
         (c2%name /= 'test')) error stop 5_4

    if ( (t2%id /= 1) .or. (t2%value /= 0.0) .or. &
         (t2%name /= 'test') .or. t2%isSet) error stop 6_4

    if ( (b1_m%id /= 1) .or. (b1_m%value /= 0.0)) error stop 7_4

    if ( (c1_m%id /= 1) .or. (c1_m%value /= 0.0) .or. &
         (c1_m%name /= 'test')) error stop 8_4

    if ( (t1_m%id /= 1) .or. (t1_m%value /= 0.0) .or. &
         (t1_m%name /= 'test') .or. t1_m%isSet) error stop 9_4

end
