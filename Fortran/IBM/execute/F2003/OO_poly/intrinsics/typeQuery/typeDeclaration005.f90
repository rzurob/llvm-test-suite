! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration005.f
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
!*  DATE                       : 10/26/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    A   : unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    MOLD: polymorphic but not unlimited polymorphic. Declared type is
!*          extensible, and can be either abstract or non-abstract.
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

program typeDeclaration005
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2
    class(Base), pointer :: b1 => null()
    class(AbstractParent), pointer :: ap1 => null()

    !-------- non abstract type

    if(extends_type_of(arg1, b1)) error stop 1_4
    if(extends_type_of(arg2, b1)) error stop 2_4
    if(same_type_as(arg1, b1)) error stop 3_4
    if(same_type_as(arg2, b1)) error stop 4_4

    allocate(Base::arg1)
    if(.NOT. extends_type_of(arg1, b1)) error stop 5_4
    if(.NOT. same_type_as(arg1, b1)) error stop 6_4
    deallocate(arg1)
    if(extends_type_of(arg1, b1)) error stop 7_4
    if(same_type_as(arg1, b1)) error stop 8_4

    !-------- abstract type

    if(extends_type_of(arg1, ap1)) error stop 9_4
    if(extends_type_of(arg2, ap1)) error stop 10_4
    if(same_type_as(arg1, ap1)) error stop 11_4
    if(same_type_as(arg2, ap1)) error stop 12_4

    allocate(Child::arg1)
    if(.NOT. extends_type_of(arg1, ap1)) error stop 13_4
    if(same_type_as(arg1, ap1)) error stop 14_4
    deallocate(arg1)
    if(extends_type_of(arg1, ap1)) error stop 15_4
    if(same_type_as(arg1, ap1)) error stop 16_4
end
