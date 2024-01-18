! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/intrinsics/transfer/structureComponent003.f
! opt variations: -qnok -qnol -qreuse=self -qreuse=base

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is a structure component, which is poly
!*  array. The object containing the component is a scalar.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base(n2,k2,k3)    ! (4,20,20,4,4)
        integer, kind :: k2,k3
        integer, len  :: n2
        integer(k2)      i
        integer(k3)      j
    end type

    type, extends(Base) :: Child(k4,n3)    ! (4,20,20,4,4,4,20)
        integer, kind :: k4
        integer, len  :: n3
        class(*), allocatable :: b2(:,:)
    end type

    type Base1(k5,n4)    ! (4,20)
        integer, kind :: k5
        integer, len  :: n4
        class(*), pointer :: j(:,:)
    end type
end module

program structureComponent003
use m
    type(Base(4,20,20,4,4)) :: b1(20)
    type(Child(4,20,20,4,4,4,20)) :: c1
    type(Base1(4,20)) :: b2

    b1 = (/ (Base(4,20,20,4,4)(i,i*2), i=1,20) /)

    allocate(c1%b2(5,5), SOURCE=reshape(b1, (/5,5/), &
     (/Base(4,20,20,4,4)(-1,-2),Base(4,20,20,4,4)(-3,-4)/), (/2,1/)))

    allocate(integer::b2%j(2,2))

    select type(name1=>transfer(c1%b2, b2%j, 20))
        type is (integer)
            print *, name1
            if(size(name1) .NE. 20) error stop 1_4
        class default
            error stop 2_4
    end select
end
