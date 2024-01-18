! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr014d2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (the use of structure
!                               constructor is illegal for a type made
!                               accessible via use association and contains
!                               private component that is not default
!                               initialized)
!*
!*  KEYWORD(S)                 :
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
        real(k2), private :: value
    end type

    type, extends (base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type (child(4,4,1,20)) :: c1_m = child(4,4,1,20) (1, 1.0, 'c1_m') ! this is OK

end module

program fconstr014d2
use m
    type (child(4,4,1,20)) :: c2 = child(4,4,1,20) (id = 3, name = 'c2')  ! this is illegal
end
