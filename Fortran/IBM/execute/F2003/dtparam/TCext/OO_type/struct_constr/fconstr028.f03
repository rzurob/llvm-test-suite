! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr028.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/23/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (derived type with
!*                               inherited type-bound proc)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: id => baseID
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,1,20)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name = ''
    end type

    type, extends(child) :: thirdGeneration(k3,n3)    ! (4,20,1,20,4,20)
        integer, kind :: k3
        integer, len  :: n3
    end type

    type (base(4,20)) :: b1_m = base(4,20) ()
    type (child(4,20,1,20)), save :: c1_m = child(4,20,1,20) (name = 'c1_m')
    type (thirdGeneration(4,20,1,20,4,20)), save :: t1_m = thirdGeneration(4,20,1,20,4,20) ('t1_m')
    contains

    subroutine baseID
        print *, 'base'
    end subroutine
end module

program fconstr028
use m

    type (thirdGeneration(4,20,1,20,4,20)) :: t1 = thirdGeneration(4,20,1,20,4,20)(name = 't1')
    type (thirdGeneration(4,20,1,20,4,20)) :: t2 = thirdGeneration(4,20,1,20,4,20)('t2')
    type (thirdGeneration(4,20,1,20,4,20)) :: t3 = thirdGeneration(4,20,1,20,4,20)()

    if (t1%name /= 't1') error stop 1_4

    if (t1%name /= t1%child%name) error stop 2_4

    if (t2%name /= 't2') error stop 3_4

    if (t2%name /= t2%child%name) error stop 4_4

    if (t3%name /= '') error stop 5_4

    if (t3%name /= t3%child%name) error stop 6_4

    if (c1_m%name /= 'c1_m') error stop 7_4

    if ((t1_m%name /= 't1_m') .or. (t1_m%name /= t1_m%child%name)) error stop 8_4

    call t3%id()
end