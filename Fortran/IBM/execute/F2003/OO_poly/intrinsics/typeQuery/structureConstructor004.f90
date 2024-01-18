! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstructor004.f
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
!*  DATE                       : 10/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MOLD: is specified using structure constructor with extensible
!*          non abstract type.
!*    A   : polymorphic but not unlimited polymorphic, declared type
!*          can be abstract or non-abstract.
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

program structureConstructor004
use m
    class(Base), pointer :: arg1 => null()
    class(AbstractParent), allocatable :: arg2

    if(extends_type_of(arg1, Child(1, "abc"))) error stop 1_4
    if(extends_type_of(arg2, Child(1, "abc"))) error stop 2_4
    if(same_type_as(arg1, Child(1, "abc"))) error stop 3_4
    if(same_type_as(arg2, Child(1, "abc"))) error stop 4_4

    allocate(Child::arg1)
    allocate(Base::arg2)

    if(.NOT. extends_type_of(arg1, Child(1, "abc"))) error stop 5_4
    if(extends_type_of(arg2, Child(1, "abc"))) error stop 6_4
    if(.NOT. same_type_as(arg1, Child(1, "abc"))) error stop 7_4
    if(same_type_as(arg2, Child(1, "abc"))) error stop 8_4
end
