! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation007.f
! %VERIFY: argAssociation007.out:argAssociation007.vf
! %STDIN:
! %STDOUT: argAssociation007.out
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
!*  is a pointer or allocatable, non-poly, and is scalar.
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
        integer j
    end type

    type Base1
        integer j
        type(Base) :: k
        integer m
    end type
end module

program argAssociation007
use m
    type(Base), pointer :: b
    type(Base1), allocatable :: b1

    allocate(b, SOURCE=Base(j=11,i=10))
    allocate(b1, SOURCE=Base1(6, Base(7,8), 9))

    call sub1(b, b1)

    contains

    subroutine sub1(arg1, arg2)
        type(Base), pointer :: arg1
        type(Base1), allocatable :: arg2

        associate(name1=>transfer(arg1, arg2))
            print *, name1%j, name1%k%i
        end associate

        print *, transfer(arg2, arg1, 2)
    end subroutine
end
