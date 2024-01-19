! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr004d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (all components without
!                               default initializations must be supplied in the
!                               structure constructor)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

end module

program fconstr004d
use m

    type (base(4)) :: b1
    type (child(4,1,20)) :: c1, c2

    b1 = base(4)()     !<-- illegal
    c1 = child(4,1,20)(1)       !<-- illegal
    c1 = child(4,1,20)(1, 'test', 10.0)     !<-- illegal
    c1 = child(4,1,20)('test')      !<-- illegal
    c2 = child(4,1,20)()        !<-- illegal
end
