! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstructor002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/26/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    A   : is unlimited polymorphic and is a disassociated pointer or
!*          an unallocated allocatable.
!*    MOLD: is specified using structure constructor with extensible
!*          non abstract type.
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
        character(10) :: c
    end type
end module

program structureConstructor002
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2

    if(extends_type_of(arg1, Base(1))) error stop 1_4
    if(extends_type_of(arg2, Base(1))) error stop 2_4
    if(same_type_as(arg1, Base(1))) error stop 3_4
    if(same_type_as(arg2, Base(1))) error stop 4_4

    allocate(Child::arg1)
    if(.NOT. extends_type_of(arg1, Base(1))) error stop 5_4
    if(same_type_as(arg1, Base(1))) error stop 6_4

    allocate(Base::arg2)
    if(.NOT. extends_type_of(arg2, Base(1))) error stop 7_4
    if(.NOT. same_type_as(arg2, Base(1))) error stop 8_4
end
