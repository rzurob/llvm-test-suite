! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer014.f
! opt variations: -ql -qdefaultpv -qreuse=self -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is array
!*    Physical representation of result has the same length as that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD, and has the same physical representation as SOURCE.
!*    Poly
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

    type Base1(k2,k3)    ! (4,4)
        integer, kind :: k2,k3
        integer(k2)      j
        integer(k3)      k(5)
    end type

    type, extends(Base) :: Child    ! (4)
        type(Base1(k1,k1)) :: b1
    end type

    type, extends(Base1) :: Child1    ! (4,4)
        type(Base(k2)) :: b2(22)
    end type
end module

program transfer014
use m
    class(Base(4)), allocatable :: src1(:,:)
    class(Base1(4,4)), pointer :: m1

    allocate(src1(2,3), SOURCE=reshape((/(Base(4)(i),i=1,6)/), (/2,3/)))
    nullify(m1)

    select type(src1)
        type is (Base(4))
            print *, src1
        class default
            error stop 1_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base1(4,4))
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(src1)
    allocate(src1(2,2), SOURCE=reshape((/(Child(4)(i, &
     Base1(4,4)(i,(/(i+j,j=1,5)/))),i=1,4)/), (/2,2/)))
    allocate(m1, SOURCE=Child1(4,4)(8, (/(i,i=1,5)/), (/(Base(4)(i),i=1,22)/)))

    select type(src1)
        type is (Child(4))
            print *, src1
        class default
            error stop 3_4
    end select

    select type(m1)
        type is (Child1(4,4))
            print *, m1
        class default
            error stop 4_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child1(4,4))
            print *, name1
        class default
            error stop 5_4
    end select
end
