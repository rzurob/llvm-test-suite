! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer003.f
! %VERIFY: transfer003.out:transfer003.vf
! %STDIN:
! %STDOUT: transfer003.out
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type Base1
        type(Base) :: b
    end type

    type, extends(Base1) :: Child1
        type(Base1) :: b1
    end type
end module

program transfer003
use m
    class(Base), allocatable :: src1
    class(Base1), pointer :: m1
    type(base1), target :: t1

    allocate(src1, SOURCE=Base(9))

    select type(src1)
        type is (Base)
            print *, src1
        class default
            error stop 1_4
    end select

    m1 => t1

    select type(name1=>transfer(src1, m1))
        type is (Base1)
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(src1)
    allocate(src1, SOURCE=Child(2,3))
    allocate(m1, SOURCE=Child1(Base(1),Base1(Base(1))))

    select type(src1)
        type is (Child)
            print *, src1
        class default
            error stop 3_4
    end select

    select type(m1)
        type is (Child1)
            print *, m1
        class default
            error stop 4_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child1)
            print *, name1
        class default
            error stop 5_4
    end select
end
