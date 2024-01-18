! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer008.f
! %VERIFY: transfer008.out:transfer008.vf
! %STDIN:
! %STDOUT: transfer008.out
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
!*    Physical representation of result has longer length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The leading part has the same physical representation as
!*  SOURCE, and the remaining part is processor dependent.
!*    Unlimited poly
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
        integer k
        integer m
    end type

    type, extends(Base1) :: Child1
        integer i
        integer j
    end type
end module

program transfer008
use m
    class(*), allocatable :: src1
    class(*), pointer :: m1

    allocate(src1, SOURCE=Base(9))
    allocate(m1, SOURCE=Base1(4,5))

    select type(src1)
        type is (Base)
            print *, src1
        class default
            error stop 1_4
    end select

    select type(m1)
        type is (Base1)
            print *, m1
        class default
            error stop 2_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base1)
            print *, name1%k
        class default
            error stop 3_4
    end select

    deallocate(src1, m1)
    allocate(src1, SOURCE=Child(5,6))
    allocate(m1, SOURCE=Child1(1,2,3,4))

    select type(src1)
        type is (Child)
            print *, src1
        class default
            error stop 4_4
    end select

    select type(m1)
        type is (Child1)
            print *, m1
        class default
            error stop 5_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child1)
            print *, name1%k, name1%m
        class default
            error stop 6_4
    end select
end
