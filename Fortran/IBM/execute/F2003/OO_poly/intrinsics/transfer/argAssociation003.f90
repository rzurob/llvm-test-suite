! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation003.f
! %VERIFY: argAssociation003.out:argAssociation003.vf
! %STDIN:
! %STDOUT: argAssociation003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, poly, and is scalar.
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

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base) :: arg1
        class(Base) :: arg2
        class(Base) :: arg3
        class(Base) :: arg4

        select type (name1=>transfer(arg2, arg3))
            type is (Child)
                print *, name1
            class default
                error stop 1_4
        end select

        select type (name1=>transfer(arg4, arg1, 2))
            type is (Base)
                print *, name1
            class default
                error stop 2_4
        end select
    end subroutine
end module

program argAssociation003
use m
    type(Base) :: b1
    type(Child) :: c1
    class(Base), pointer :: b2
    class(Child), allocatable :: c2

    b1 = Base(10)
    c1 = Child(7, 8)
    allocate(b2, SOURCE=Child(3,4))
    allocate(c2, SOURCE=Child(j=5,i=6))

    call sub1(b1, c1, b2, c2)
end
