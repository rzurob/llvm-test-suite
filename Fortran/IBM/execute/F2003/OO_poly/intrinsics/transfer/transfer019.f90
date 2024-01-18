! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer019.f
! %VERIFY: transfer019.out:transfer019.vf
! %STDIN:
! %STDOUT: transfer019.out
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
!*    Physical representation of result has shorter length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The physical representation is the leading part of that of
!*  SOURCE.
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
    type Base
        integer i
    end type

    type Base1
        integer j(20)
    end type

    type, extends(Base) :: Child
        integer j(3,2)
    end type

    type, extends(Base1) :: Child1
        type(Base) :: b2(20)
    end type
end module

program transfer019
use m
    class(Base), allocatable :: src1(:,:)
    class(*), pointer :: m1

    allocate(src1(3,3), SOURCE=reshape((/(Base(i),i=1,6)/), &
     (/3,3/), (/Base(-1)/), (/2,1/)))
    allocate(Child::m1)

    select type(name1=>transfer(src1, m1))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select

    deallocate(src1, m1)
    allocate(src1(3,2), SOURCE=reshape((/(Child(i, &
     reshape((/(i+j,j=1,6)/),(/3,2/))), i=1,6)/), (/3,2/)))
    allocate(Child1::m1)

    select type(name1=>transfer(src1, m1))
        type is (Child1)
            print *, name1%j
            print *, name1%b2
        class default
            error stop 2_4
    end select
end
