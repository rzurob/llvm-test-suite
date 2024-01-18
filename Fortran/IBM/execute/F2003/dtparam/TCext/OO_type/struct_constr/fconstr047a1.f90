! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qdefaultpv -qreuse=self /tstdev/OO_type/struct_constr/fconstr047a1.f
! opt variations: -qnock -qnok -ql -qnodefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (for component with
!*                               partial default initialization)
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
    type A(k1,n1,k2)    ! (1,20,4)
        integer, kind             :: k1,k2
        integer, len              :: n1
        character(kind=k1,len=n1) :: name = 'no-name'
        integer(k2)               :: id
    end type
end module

module m1
use m
    type B(k3)    ! (4)
        integer, kind    :: k3
        type(A(1,20,k3)) :: a1 = A(1,20,k3) (id = 1)
    end type
end module

program fconstr047a1
use m1
    type (B(4)) :: b1

    type (B(4)) :: b2 = B(4) (a1 = A(1,20,4) (name = 'b2', id = 10))

    if ((b1%a1%name /= 'no-name') .or. (b1%a1%id /= 1)) error stop 1_4

    if ((b2%a1%name /= 'b2') .or. (b2%a1%id /= 10)) error stop 2_4

    b1 = B(4)()

    if ((b1%a1%name /= 'no-name') .or. (b1%a1%id /= 1)) error stop 3_4

    b1 = B(4) (A(1,20,4)(id = -1))

    if ((b1%a1%name /= 'no-name') .or. (b1%a1%id /= -1)) error stop 4_4
end
