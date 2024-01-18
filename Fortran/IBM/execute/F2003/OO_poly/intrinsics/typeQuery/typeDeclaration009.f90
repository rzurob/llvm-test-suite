! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration009.f
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
!*    MOLD: non polymorphic and declared type is extensible.
!*    A   : unlimited polymorphic, but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is extensible.
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

program typeDeclaration009
use m
    type(Base) :: mold1
    type(Child), target :: mold2
    type(Base), pointer :: mold3 => null()
    type(Child), allocatable :: mold4
    type(Base), parameter :: mold5 = Base(1)

    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2

    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg1, mold3)) error stop 3_4
    if(extends_type_of(arg1, mold4)) error stop 4_4
    if(extends_type_of(arg1, mold5)) error stop 5_4
    if(extends_type_of(arg2, mold1)) error stop 6_4
    if(extends_type_of(arg2, mold2)) error stop 7_4
    if(extends_type_of(arg2, mold3)) error stop 8_4
    if(extends_type_of(arg2, mold4)) error stop 9_4
    if(extends_type_of(arg2, mold5)) error stop 10_4

    if(same_type_as(arg1, mold1)) error stop 11_4
    if(same_type_as(arg1, mold2)) error stop 12_4
    if(same_type_as(arg1, mold3)) error stop 13_4
    if(same_type_as(arg1, mold4)) error stop 14_4
    if(same_type_as(arg1, mold5)) error stop 15_4
    if(same_type_as(arg2, mold1)) error stop 16_4
    if(same_type_as(arg2, mold2)) error stop 17_4
    if(same_type_as(arg2, mold3)) error stop 18_4
    if(same_type_as(arg2, mold4)) error stop 19_4
    if(same_type_as(arg2, mold5)) error stop 20_4

    allocate(Base::arg1)
    allocate(Child::arg2)

    if(.NOT. extends_type_of(arg1, mold1)) error stop 21_4
    if(extends_type_of(arg1, mold2)) error stop 22_4
    if(.NOT. extends_type_of(arg1, mold3)) error stop 23_4
    if(extends_type_of(arg1, mold4)) error stop 24_4
    if(.NOT. extends_type_of(arg1, mold5)) error stop 25_4
    if(.NOT. extends_type_of(arg2, mold1)) error stop 26_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 27_4
    if(.NOT. extends_type_of(arg2, mold3)) error stop 28_4
    if(.NOT. extends_type_of(arg2, mold4)) error stop 29_4
    if(.NOT. extends_type_of(arg2, mold5)) error stop 30_4

    if(.NOT. same_type_as(arg1, mold1)) error stop 31_4
    if(same_type_as(arg1, mold2)) error stop 32_4
    if(.NOT. same_type_as(arg1, mold3)) error stop 33_4
    if(same_type_as(arg1, mold4)) error stop 34_4
    if(.NOT. same_type_as(arg1, mold5)) error stop 35_4
    if(same_type_as(arg2, mold1)) error stop 36_4
    if(.NOT. same_type_as(arg2, mold2)) error stop 37_4
    if(same_type_as(arg2, mold3)) error stop 38_4
    if(.NOT. same_type_as(arg2, mold4)) error stop 39_4
    if(same_type_as(arg2, mold5)) error stop 40_4
end
