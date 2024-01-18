! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation005.f
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
!*    arguments of another procedure. Dummy arguments are non-poly,
!*    non-pointer, and non-allocatable. Dummy arguments are explicit-
!*    shape or assumed-shape arrays. Actual arguments are explicit-
!*    shape or deferred-shape arrays.
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
end module

program argAssociation005
use m
    type(Base) :: b1(3,5)
    class(Base), pointer :: b2(:) => null()
    type(Child) :: c1(8)
    class(Child), allocatable :: c2(:,:)

    allocate(Child::b2(4))
    allocate(c2(3,6))
    call sub1(b1(1:,:), b2(2), c1(2:4), c2(:,:))

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base) :: arg1(*)
        type(Base) :: arg2
        type(Child) :: arg3(2)
        type(Child) :: arg4(2,2,2)

        if(.NOT. extends_type_of(arg1, arg2)) error stop 1_4
        if(.NOT. extends_type_of(arg2, arg1)) error stop 2_4
        if(extends_type_of(arg2, arg3(1:))) error stop 3_4
        if(.NOT. extends_type_of(arg3(:), arg2)) error stop 4_4
        if(.NOT. extends_type_of(arg3, arg4(1:,:,6:4))) error stop 5_4
        if(.NOT. extends_type_of(arg4(:,:,2), arg3(1:))) error stop 6_4

        if(.NOT. same_type_as(arg1, arg2)) error stop 7_4
        if(same_type_as(arg2, arg3(1:))) error stop 8_4
        if(.NOT. same_type_as(arg3(1:), arg4(:,1:,6:4))) error stop 9_4
    end subroutine
end
