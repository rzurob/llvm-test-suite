! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer011.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer011.f
! %VERIFY: transfer011.out:transfer011.vf
! %STDIN:
! %STDOUT: transfer011.out
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
!*    SOURCE is scalar
!*    Physical representation of result has shorter length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The physical representation is the leading part of that of
!*  SOURCE.
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
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      k
        integer(k2)      m
    end type

    type, extends(Base1) :: Child1    ! (20,4)
        integer(k2) i
        integer(k2) j
    end type
end module

program transfer011
use m
    class(Base1(:,4)), allocatable :: src1
    class(Base(:,4)), pointer :: m1

    allocate(src1, SOURCE=Base1(20,4)(8,9))

    select type(src1)
        type is (Base1(*,4))
            print *, src1
        class default
            error stop 1_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base(*,4))
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(src1)
    allocate(src1, SOURCE=Child1(20,4)(4,5,6,7))
    allocate(m1, SOURCE=Child(20,4)(3,4))

    select type(src1)
        type is (Child1(*,4))
            print *, src1
        class default
            error stop 3_4
    end select

    select type(m1)
        type is (Child(*,4))
            print *, m1
        class default
            error stop 4_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child(*,4))
            print *, name1
        class default
            error stop 5_4
    end select
end
