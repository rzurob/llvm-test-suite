! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/transfer/transfer017.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer017.f
! %VERIFY: transfer017.out:transfer017.vf
! %STDIN:
! %STDOUT: transfer017.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is array
!*    Physical representation of result has longer length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The leading part has the same physical representation as
!*  SOURCE, and the remaining part is processor dependent.
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

    type Base1(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j(20)
    end type

    type, extends(Base) :: Child(n3,k3)    ! (20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j(3,2)
    end type

    type, extends(Base1) :: Child1(n4,k4)    ! (20,4,20,4)
        integer, kind     :: k4
        integer, len      :: n4
        type(Base(n4,k4)) :: b2(20)
    end type
end module

program transfer017
use m
    class(Base(:,4)), allocatable :: src1(:,:)
    class(*), pointer :: m1

    allocate(src1(2,3), SOURCE=reshape((/(Base(20,4)(i),i=1,6)/), (/2,3/)))
    allocate(Child(20,4,20,4)::m1)

    select type(src1)
        type is (Base(*,4))
            print *, src1
        class default
            error stop 1_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child(*,4,*,4))
            print *, name1%i
            print *, name1%j(:,1), name1%j(1:2,2)
        class default
            error stop 2_4
    end select

    deallocate(src1, m1)
    allocate(src1(2,2), SOURCE=reshape((/(Child(20,4,20,4)(i, &
     reshape((/(i+j,j=1,6)/),(/3,2/))), i=1,4)/), (/2,2/)))
    allocate(Child1(20,4,20,4)::m1)

    select type(src1)
        type is (Child(*,4,*,4))
            print *, src1
        class default
            error stop 3_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child1(*,4,*,4))
            print *, name1%j
            print *, name1%b2(1:8)
        class default
            error stop 4_4
    end select
end
