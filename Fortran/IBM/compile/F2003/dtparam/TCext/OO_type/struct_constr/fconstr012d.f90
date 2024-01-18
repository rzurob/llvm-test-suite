! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr012d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C484)
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
end module

program fconstr012d
use m

    type (base(4,4)) :: b1
    type (child(4,4,1,20)) :: c1, c2
    type (thirdGeneration(4,4,1,20,1)) :: t1

    t1 = thirdGeneration(4,4,1,20,1) (child = c1, name = 't1')
end
