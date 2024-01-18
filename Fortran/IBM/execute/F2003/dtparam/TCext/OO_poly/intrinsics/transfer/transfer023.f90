! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_poly/intrinsics/transfer/transfer023.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is present
!*    MOLD is scalar or array
!*    SOURCE is array
!*    The result is a rank one array of the same type and type
!*  parameters as MOLD. Its size is the specified SIZE. Its physical
!*  representation matches that of SOURCE, possibly with the remainder
!*  processor dependent.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type Base1(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        integer(k2)          k
        type(Base(n2,k2)) :: b
        integer(k2)          n
    end type
end module

program transfer023
use m
    class(*), allocatable :: src1(:,:,:)
    class(Base1(4,:)), pointer :: m1
    class(*), pointer :: m2(:,:)

    allocate(src1(2,2,2), SOURCE=reshape((/(Base(20,4)(i),i=2,9)/), &
     (/2,2,2/), (/Base(20,4)(-1)/), (/3,2,1/)))

    select type(name1=>transfer(src1, m1, 2))
        type is (Base1(4,*))
            print *, name1
        class default
            error stop 1_4
    end select

    allocate(Base1(4,20)::m2(4,3))

    select type(name1=>transfer(src1, m2, 3))
        type is (Base1(4,*))
            print *, name1(1:2), name1(3)%k, name1(3)%b
        class default
            error stop 2_4
    end select
end
