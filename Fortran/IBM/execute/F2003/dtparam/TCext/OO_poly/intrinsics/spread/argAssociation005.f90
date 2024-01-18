! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 01/20/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, unlimited poly, and is scalar.
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

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1
        class(*) :: arg2
        class(*) :: arg3
        class(*) :: arg4
        class(*) :: arg5

        select type(name1=>spread(arg1, 1, 2))
            type is (Base(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>spread(arg2, 1, 3))
            type is (Child(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select

        select type(name1=>spread(arg3, 1, 4))
            type is (Child(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 3_4
        end select

        select type(name1=>spread(arg4, 1, 5))
            type is (Child(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 4_4
        end select

        select type(name1=>spread(arg5, 1, 6))
            type is (Child(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 5_4
        end select
    end subroutine
end module

program argAssociation005
use m
    type(Base(20,4)) :: b1
    type(Child(20,4)) :: c1
    class(Base(:,4)), pointer :: b2
    class(Child(:,4)), allocatable :: c2
    class(*), allocatable :: u1

    b1 = Base(20,4)(10)
    c1 = Child(20,4)(7, 8)
    allocate(b2, SOURCE=Child(20,4)(3,4))
    allocate(c2, SOURCE=Child(20,4)(j=5,i=6))
    allocate(u1, SOURCE=Child(20,4)(13,14))

    call sub1(b1, c1, b2, c2, u1)
end
