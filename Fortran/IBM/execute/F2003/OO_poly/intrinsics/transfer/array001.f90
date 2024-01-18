! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MOLD is array element or array section
!*    Poly and unlimited poly
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type Base1
        integer k(2)
        type(Base) :: b
        integer j(3)
    end type
end module

program array001
use m
    class(Base1), allocatable :: src1
    class(Base1), pointer :: src2(:,:,:)
    class(*), pointer :: m1(:,:)

    allocate(src1, SOURCE=Base1((/1,2/),Base(3),(/4,5,6/)))
    allocate(Base::m1(2,2))

    select type(name1=>transfer(src1, m1(1,2)))
        type is (Base)
            print *, name1
        class default
            error stop 1_4
    end select

    deallocate(m1)
    allocate(Child::m1(2,2))
    allocate(src2(2,2,2), SOURCE= &
     reshape((/(Base1((/i-1,i+1/),Base(i),(/i,i+1,i+2/)),i=1,8)/), &
     (/2,2,2/)))

    select type(name1=>transfer(src2(1:,2,:), m1(1:,:), 10))
        type is (Child)
            print *, name1
        class default
            error stop 2_4
    end select
end
