! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Arguments of the intrinsics are dummy
!*    arguments of another procedure. Dummy arguments are poly,
!*    non-pointer, and non-allocatable.
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(AbstractParent) :: arg1
        class(AbstractParent) :: arg2
        class(Base) :: arg3
        class(Base) :: arg4

        if(extends_type_of(arg1, arg2)) error stop 1_4
        if(.NOT. extends_type_of(arg2, arg1)) error stop 2_4
        if(.NOT. extends_type_of(arg2, arg3)) error stop 3_4
        if(extends_type_of(arg3, arg2)) error stop 4_4
        if(extends_type_of(arg3, arg4)) error stop 5_4
        if(.NOT. extends_type_of(arg4, arg3)) error stop 6_4

        if(same_type_as(arg1, arg2)) error stop 7_4
        if(.NOT. same_type_as(arg1, arg3)) error stop 8_4
        if(same_type_as(arg1, arg4)) error stop 9_4
        if(same_type_as(arg2, arg3)) error stop 10_4
        if(.NOT. same_type_as(arg2, arg4)) error stop 11_4
        if(same_type_as(arg3, arg4)) error stop 12_4
    end subroutine
end module

program argAssociation002
use m
    type(Base) :: b1
    type(Child) :: c1
    class(AbstractParent), allocatable :: ap1
    class(Base), pointer :: b2 => null()

    allocate(Base::ap1)
    allocate(Child::b2)
    call sub1(ap1, b2, b1, c1)
end
