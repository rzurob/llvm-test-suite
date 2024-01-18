! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation009.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation009.f
! %VERIFY: argAssociation009.out:argAssociation009.vf
! %STDIN:
! %STDOUT: argAssociation009.out
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
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, poly, and is scalar.
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
end module

program argAssociation009
use m
    class(Base(:,4)), pointer :: b1
    class(Base(:,4)), allocatable :: b2

    allocate(b1, SOURCE=Base(20,4)(10))
    allocate(b2, SOURCE=Child(20,4)(j=7, i=6))

    call sub1(b1, b2)

    contains

    subroutine sub1(arg1, arg2)
        class(Base(:,4)), pointer :: arg1
        class(Base(:,4)), allocatable :: arg2

        select type(name1=>spread(arg1, 1, 6))
            type is (Base(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>spread(arg2, 1, 5))
            type is (Child(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select
    end subroutine
end
