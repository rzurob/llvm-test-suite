! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr008d.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (diagnostic test case
!                               for re-using component that is inherited)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends (base) :: child(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type

    type, extends (child) :: thirdGeneration(k3)    ! (4,20,4,20,4)
        integer, kind :: k3
        integer(k3)   :: id
    end type
end module

program fconstr008d
use m

    type (thirdGeneration(4,20,4,20,4)) :: t1 = thirdGeneration(4,20,4,20,4) (base = base(4,20)(), &
                child = child(4,20,4,20) (), id = 2)

    !! strange as it appears, the next structure constructor is legal
    type (thirdGeneration(4,20,4,20,4)) :: t2 = thirdGeneration(4,20,4,20,4) (2, base = base(4,20)())

    type (thirdGeneration(4,20,4,20,4)) :: t3 = thirdGeneration(4,20,4,20,4) (child = child(4,20,4,20)(), 3)
end
