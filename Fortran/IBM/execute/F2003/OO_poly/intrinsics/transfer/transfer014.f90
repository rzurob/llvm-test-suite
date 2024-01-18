! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer014.f
! %VERIFY: transfer014.out:transfer014.vf
! %STDIN:
! %STDOUT: transfer014.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/16/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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
    type Base
        integer i
    end type

    type Base1
        integer j
        integer k(5)
    end type

    type, extends(Base) :: Child
        type(Base1) :: b1
    end type

    type, extends(Base1) :: Child1
        type(Base) :: b2(22)
    end type
end module

program transfer014
use m
    class(Base), allocatable :: src1(:,:)
    class(Base1), pointer :: m1

    allocate(src1(2,3), SOURCE=reshape((/(Base(i),i=1,6)/), (/2,3/)))
    nullify(m1)

    select type(src1)
        type is (Base)
            print *, src1
        class default
            error stop 1_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base1)
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(src1)
    allocate(src1(2,2), SOURCE=reshape((/(Child(i, &
     Base1(i,(/(i+j,j=1,5)/))),i=1,4)/), (/2,2/)))
    allocate(m1, SOURCE=Child1(8, (/(i,i=1,5)/), (/(Base(i),i=1,22)/)))

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
