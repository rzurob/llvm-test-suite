! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (for array components,
!                               the source-expr must be in shape conformance)
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
    type base
        integer*4 ::id
        real*4 :: value(2)
    end type

end module


program fconstr023d
use m
    type (base) :: b1

    b1 = base (1, (/1.0, 2.0, 3.0/))

    b1 = base (1, (/10.0/))
end