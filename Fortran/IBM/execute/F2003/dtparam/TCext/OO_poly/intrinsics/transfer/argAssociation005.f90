! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/argAssociation005.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation005.f
! %VERIFY: argAssociation005.out:argAssociation005.vf
! %STDIN:
! %STDOUT: argAssociation005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, unlimited poly, and is scalar.
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

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       j
        type(Base(k2)) :: k
        integer(k2)       m
        integer(k2)       n
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1
        class(*) :: arg2
        class(*) :: arg3
        class(*) :: arg4
        class(*) :: arg5

        select type (name1=>transfer(arg2, arg1))
            type is (Base(4))
                print *, name1
            class default
                error stop 1_4
        end select

        associate(name1=>transfer(arg3, Child(4)(1,2)))
            print *, name1
            if(.NOT. same_type_as(name1, Child(4)(1,2))) error stop 2_4
        end associate

        associate(name1=>transfer(arg4, (/Child(4)(1,2)/)))
            print *, name1
            if(.NOT. same_type_as(name1, Child(4)(1,2))) error stop 3_4
        end associate

        select type (name1=>transfer(arg5, arg3, 2))
            type is (Child(4))
                print *, name1
            class default
                error stop 4_4
        end select

        select type (name1=>transfer(arg5, arg1, 3))
            type is (Base(4))
                print *, name1
            class default
                error stop 5_4
        end select
    end subroutine
end module

program argAssociation005
use m
    type(Base(4)) :: b1
    type(Child(4)) :: c1
    class(Base(4)), pointer :: b2
    class(Child(4)), allocatable :: c2
    class(*), allocatable :: u1

    b1 = Base(4)(10)
    c1 = Child(4)(7, 8)
    allocate(b2, SOURCE=Child(4)(3,4))
    allocate(c2, SOURCE=Child(4)(j=5,i=6))
    allocate(u1, SOURCE=Base1(4)(11,Base(4)(12),13,14))

    call sub1(b1, c1, b2, c2, u1)
end
