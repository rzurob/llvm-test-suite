! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer008.f
! opt variations: -ql -qreuse=self -qreuse=none

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    type Base1(k2,k3)    ! (4,4)
        integer, kind :: k2,k3
        integer(k2)      k
        integer(k3)      m
    end type

    type, extends(Base1) :: Child1    ! (4,4)
        integer(k2) i
        integer(k2) j
    end type
end module

program transfer008
use m
    class(*), allocatable :: src1
    class(*), pointer :: m1

    allocate(src1, SOURCE=Base(4)(9))
    allocate(m1, SOURCE=Base1(4,4)(4,5))

    select type(src1)
        type is (Base(4))
            print *, src1
        class default
            error stop 1_4
    end select

    select type(m1)
        type is (Base1(4,4))
            print *, m1
        class default
            error stop 2_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base1(4,4))
            print *, name1%k
        class default
            error stop 3_4
    end select

    deallocate(src1, m1)
    allocate(src1, SOURCE=Child(4)(5,6))
    allocate(m1, SOURCE=Child1(4,4)(1,2,3,4))

    select type(src1)
        type is (Child(4))
            print *, src1
        class default
            error stop 4_4
    end select

    select type(m1)
        type is (Child1(4,4))
            print *, m1
        class default
            error stop 5_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child1(4,4))
            print *, name1%k, name1%m
        class default
            error stop 6_4
    end select
end
