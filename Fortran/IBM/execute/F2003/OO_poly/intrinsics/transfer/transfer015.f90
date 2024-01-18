! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer015.f
! %VERIFY: transfer015.out:transfer015.vf
! %STDIN:
! %STDOUT: transfer015.out
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
!*    Physical representation of result has the same length as that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD, and has the same physical representation as SOURCE.
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

    type Base1
        integer j
        integer k(5)
    end type

    type, extends(Base) :: Child
        type(Base1) :: b1(5)
        integer n
    end type

    type, extends(Base1) :: Child1
        type(Base) :: b2(2)
    end type
end module

program transfer015
use m
    class(*), allocatable :: src1(:,:)
    class(*), pointer :: m1

    allocate(src1(2,3), SOURCE=reshape((/(Base(i),i=1,6)/), (/2,3/)))
    allocate(m1, SOURCE=Base1(8, (/(i,i=3,7)/)))

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
            print *, name1
        class default
            error stop 3_4
    end select

    deallocate(src1, m1)
    allocate(src1(2,2), SOURCE=reshape((/(Child1(20+i, &
     (/(i+j,j=1,5)/), (/Base(11-i),Base(12+i)/)),i=1,4)/), (/2,2/), &
     (/Child1(1,(/1,2,3,4,5/),(/Base(1),Base(2)/))/), (/2,1/)))
    allocate(m1, SOURCE=Child(1, &
     (/(Base1(i,(/(i+j,j=1,5)/)),i=1,5)/), 2))

    select type(src1)
        type is (Child1)
            print *, src1
        class default
            error stop 4_4
    end select

    select type(m1)
        type is (Child)
            print *, m1
        class default
            error stop 5_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child)
            print *, name1
        class default
            error stop 6_4
    end select
end
