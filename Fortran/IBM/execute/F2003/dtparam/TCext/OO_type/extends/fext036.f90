! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 11, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (componet ordering in
!*                               intrinsic IO)
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
    type base(k1,k2)
        integer, kind :: k1,k2
        integer(k1) :: i1 = 100
        real(k2) :: r(2) = (/1.0, 2.0/)
    end type

    type, extends(base) :: child(n,k3)
        integer, len :: n
        integer, kind :: k3
        character(n) :: c2 = 'Child type'
        logical(k3) :: l2(2) = (/.true., .true./)
    end type

    type, extends(child) :: thirdGeneration(k4,k5,k6)
        integer, kind :: k4,k5,k6
        complex(k4) :: c3 = (10.0, 5.0)
        real(k5) :: r3 = -1.0d0
        integer(k6) :: i3 = 10000
    end type

end module

program fext036

use m

    type (child(4,4,20,2)) :: c1
    type (thirdGeneration(4,4,20,2,4,8,8)) :: t1

    t1%c2 = 'third Generation'

    print *, "c1's component ordering"
    print *, c1

    print *, "t1's component ordering"
    print *, t1
end
