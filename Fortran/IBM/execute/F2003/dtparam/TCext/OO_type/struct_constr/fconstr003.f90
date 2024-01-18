! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr003.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (default construction)
!*                               no initialization for any component
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
        integer, kind     :: k1,k2
        integer(k1)       :: id
        real(k2), pointer :: value
    end type

    type, extends(base) :: child(k3,n1,k4)    ! (4,4,1,20,8)
        integer, kind             :: k3,k4
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
        real(k4), allocatable     :: extendedValue (:)
    end type

    type (base(4,4)), save :: b1_m
    type (child(4,4,1,20,8)), save :: c1_m

end module

program fconstr003
use m

    type, extends (base) :: secondChild(k5)    ! (4,4,4)
        integer, kind     :: k5
        real(k5), pointer :: extendedValue (:)
    end type

    type (base(4,4)) :: b1
    type (child(4,4,1,20,8)) :: c1
    type (secondChild(4,4,4)) :: s1

end
