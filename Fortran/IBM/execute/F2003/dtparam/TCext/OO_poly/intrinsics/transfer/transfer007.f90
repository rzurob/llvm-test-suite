! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer007.f
! %VERIFY: transfer007.out:transfer007.vf
! %STDIN:
! %STDOUT: transfer007.out
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

program transfer007
use m
    class(Base(:,4)), allocatable :: src1
    class(Base1(:,4)), pointer :: m1

    allocate(src1, SOURCE=Base(20,4)(9))

    select type(src1)
        type is (Base(*,4))
            print *, src1
        class default
            error stop 1_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base1(*,4))
            print *, name1%k
        class default
            error stop 2_4
    end select

    deallocate(src1)
    allocate(src1, SOURCE=Child(20,4)(2,3))
    allocate(m1, SOURCE=Child1(20,4)(4,5,6,7))

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
            print *, name1%k, name1%m
        class default
            error stop 5_4
    end select
end
