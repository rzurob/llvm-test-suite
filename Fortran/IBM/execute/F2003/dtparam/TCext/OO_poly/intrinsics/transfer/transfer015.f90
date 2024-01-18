! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer015.f
! opt variations: -qnol -qdefaultpv -qreuse=self -qreuse=none

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type Base1(n2,k2,k3)    ! (20,4,4)
        integer, kind :: k2,k3
        integer, len  :: n2
        integer(k2)      j
        integer(k3)      k(5)
    end type

    type, extends(Base) :: Child    ! (20,4)
        type(Base1(n1,k1,k1)) :: b1(5)
        integer(k1) n
    end type

    type, extends(Base1) :: Child1    ! (20,4,4)
        type(Base(n2,k2)) :: b2(2)
    end type
end module

program transfer015
use m
    class(*), allocatable :: src1(:,:)
    class(*), pointer :: m1

    allocate(src1(2,3), SOURCE=reshape((/(Base(20,4)(i),i=1,6)/), (/2,3/)))
    allocate(m1, SOURCE=Base1(20,4,4)(8, (/(i,i=3,7)/)))

    select type(src1)
        type is (Base(*,4))
            print *, src1
        class default
            error stop 1_4
    end select

    select type(m1)
        type is (Base1(*,4,4))
            print *, m1
        class default
            error stop 2_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base1(*,4,4))
            print *, name1
        class default
            error stop 3_4
    end select

    deallocate(src1, m1)
    allocate(src1(2,2), SOURCE=reshape((/(Child1(20,4,4)(20+i, &
     (/(i+j,j=1,5)/), (/Base(20,4)(11-i),Base(20,4)(12+i)/)),i=1,4)/), (/2,2/), &
     (/Child1(20,4,4)(1,(/1,2,3,4,5/),(/Base(20,4)(1),Base(20,4)(2)/))/), (/2,1/)))
    allocate(m1, SOURCE=Child(20,4)(1, &
     (/(Base1(20,4,4)(i,(/(i+j,j=1,5)/)),i=1,5)/), 2))

    select type(src1)
        type is (Child1(*,4,4))
            print *, src1
        class default
            error stop 4_4
    end select

    select type(m1)
        type is (Child(*,4))
            print *, m1
        class default
            error stop 5_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child(*,4))
            print *, name1
        class default
            error stop 6_4
    end select
end
