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
!*  DATE                       : 01/20/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, non-poly, and is scalar.
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

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation007
use m
    type(Base), pointer :: b
    type(Child), allocatable :: c

    allocate(b, SOURCE=Base(i=10))
    allocate(c, SOURCE=Child(j=6, i=9))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base), pointer :: arg1
        type(Child), allocatable :: arg2

        associate(name1=>spread(arg1, 1, 6))
            if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate

        associate(name1=>spread(arg2, 1, 4))
            if(.NOT. same_type_as(name1, Child(1,1))) error stop 2_4
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate
    end subroutine
end
