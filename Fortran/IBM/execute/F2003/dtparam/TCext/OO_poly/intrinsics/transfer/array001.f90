! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/array001.f
! opt variations: -ql -qdefaultpv -qreuse=none

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       k(2)
        type(Base(k2)) :: b
        integer(k2)       j(3)
    end type
end module

program array001
use m
    class(Base1(4)), allocatable :: src1
    class(Base1(4)), pointer :: src2(:,:,:)
    class(*), pointer :: m1(:,:)

    allocate(src1, SOURCE=Base1(4)((/1,2/),Base(4)(3),(/4,5,6/)))
    allocate(Base(4)::m1(2,2))

    select type(name1=>transfer(src1, m1(1,2)))
        type is (Base(4))
            print *, name1
        class default
            error stop 1_4
    end select

    deallocate(m1)
    allocate(Child(4)::m1(2,2))
    allocate(src2(2,2,2), SOURCE= &
     reshape((/(Base1(4)((/i-1,i+1/),Base(4)(i),(/i,i+1,i+2/)),i=1,8)/), &
     (/2,2,2/)))

    select type(name1=>transfer(src2(1:,2,:), m1(1:,:), 10))
        type is (Child(4))
            print *, name1
        class default
            error stop 2_4
    end select
end
