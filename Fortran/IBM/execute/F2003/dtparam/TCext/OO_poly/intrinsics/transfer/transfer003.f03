! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer003.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is scalar
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    type Base1(n2,k2)    ! (20,4)
        integer, kind     :: k2
        integer, len      :: n2
        type(Base(n2,k2)) :: b
    end type

    type, extends(Base1) :: Child1    ! (20,4)
        type(Base1(n2,k2)) :: b1
    end type
end module

program transfer003
use m
    class(Base(:,4)), allocatable :: src1
    class(Base1(:,4)), pointer :: m1
    type(base1(:,4)), allocatable, target :: t1

    allocate(src1, SOURCE=Base(20,4)(9))

    select type(src1)
        type is (Base(*,4))
            print *, src1
        class default
            error stop 1_4
    end select

    allocate (base1(1,4) :: t1)

    m1 => t1

    select type(name1=>transfer(src1, m1))
        type is (Base1(*,4))
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(src1)
    allocate(src1, SOURCE=Child(20,4)(2,3))
    allocate(m1, SOURCE=Child1(20,4)(Base(20,4)(1),Base1(20,4)(Base(20,4)(1))))

    select type(src1)
        type is (Child(*,4))
            print *, src1
        class default
            error stop 3_4
    end select

    select type(m1)
        type is (Child1(*,4))
            print *, m1
        class default
            error stop 4_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child1(*,4))
            print *, name1
        class default
            error stop 5_4
    end select
end