! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration019.f
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
!*    MOLD: unlimited polymorphic but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is non-extensible.
!*    A   : unlimited polymorphic but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is non-extensible.
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

    type SequenceBase
        sequence
        integer i
        integer j
    end type
end module

program typeDeclaration019
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2
    class(*), pointer :: arg3 => null()
    class(*), allocatable :: arg4
    class(*), pointer :: arg5 => null()
    class(*), allocatable :: arg6

    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2
    class(*), pointer :: mold3 => null()
    class(*), allocatable :: mold4
    class(*), pointer :: mold5 => null()
    class(*), allocatable :: mold6

    allocate(integer(4)::arg1)
    allocate(integer(8)::arg2)
    allocate(character(5)::arg3)
    allocate(character(6)::arg4)
    allocate(SequenceBase::arg5)
    allocate(Base::arg6)
    allocate(integer(4)::mold1)
    allocate(integer(8)::mold2)
    allocate(character(5)::mold3)
    allocate(character(6)::mold4)
    allocate(SequenceBase::mold5)
    allocate(Base::mold6)

    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg1, mold3)) error stop 3_4
    if(extends_type_of(arg1, mold4)) error stop 4_4
    if(extends_type_of(arg1, mold5)) error stop 5_4
    if(extends_type_of(arg1, mold6)) error stop 6_4
    if(extends_type_of(arg2, mold1)) error stop 7_4
    if(extends_type_of(arg2, mold2)) error stop 8_4
    if(extends_type_of(arg2, mold3)) error stop 9_4
    if(extends_type_of(arg2, mold4)) error stop 10_4
    if(extends_type_of(arg2, mold5)) error stop 11_4
    if(extends_type_of(arg2, mold6)) error stop 12_4
    if(extends_type_of(arg3, mold1)) error stop 13_4
    if(extends_type_of(arg3, mold2)) error stop 14_4
    if(extends_type_of(arg3, mold3)) error stop 15_4
    if(extends_type_of(arg3, mold4)) error stop 16_4
    if(extends_type_of(arg3, mold5)) error stop 17_4
    if(extends_type_of(arg3, mold6)) error stop 18_4
    if(extends_type_of(arg4, mold1)) error stop 19_4
    if(extends_type_of(arg4, mold2)) error stop 20_4
    if(extends_type_of(arg4, mold3)) error stop 21_4
    if(extends_type_of(arg4, mold4)) error stop 22_4
    if(extends_type_of(arg4, mold5)) error stop 23_4
    if(extends_type_of(arg4, mold6)) error stop 24_4
    if(extends_type_of(arg5, mold1)) error stop 25_4
    if(extends_type_of(arg5, mold2)) error stop 26_4
    if(extends_type_of(arg5, mold3)) error stop 27_4
    if(extends_type_of(arg5, mold4)) error stop 28_4
    if(extends_type_of(arg5, mold5)) error stop 29_4
    if(extends_type_of(arg5, mold6)) error stop 30_4
    if(extends_type_of(arg6, mold1)) error stop 31_4
    if(extends_type_of(arg6, mold2)) error stop 32_4
    if(extends_type_of(arg6, mold3)) error stop 33_4
    if(extends_type_of(arg6, mold4)) error stop 34_4
    if(extends_type_of(arg6, mold5)) error stop 35_4
    if(.NOT. extends_type_of(arg6, mold6)) error stop 36_4

    if(.NOT. same_type_as(arg1, mold1)) error stop 37_4
    if(same_type_as(arg1, mold2)) error stop 38_4
    if(same_type_as(arg1, mold3)) error stop 39_4
    if(same_type_as(arg1, mold4)) error stop 40_4
    if(same_type_as(arg1, mold5)) error stop 41_4
    if(same_type_as(arg1, mold6)) error stop 42_4
    if(same_type_as(arg2, mold1)) error stop 43_4
    if(.NOT. same_type_as(arg2, mold2)) error stop 44_4
    if(same_type_as(arg2, mold3)) error stop 45_4
    if(same_type_as(arg2, mold4)) error stop 46_4
    if(same_type_as(arg2, mold5)) error stop 47_4
    if(same_type_as(arg2, mold6)) error stop 48_4
    if(same_type_as(arg3, mold1)) error stop 49_4
    if(same_type_as(arg3, mold2)) error stop 50_4
    if(.NOT. same_type_as(arg3, mold3)) error stop 51_4
    if(same_type_as(arg3, mold4)) error stop 52_4
    if(same_type_as(arg3, mold5)) error stop 53_4
    if(same_type_as(arg3, mold6)) error stop 54_4
    if(same_type_as(arg4, mold1)) error stop 55_4
    if(same_type_as(arg4, mold2)) error stop 56_4
    if(same_type_as(arg4, mold3)) error stop 57_4
    if(.NOT. same_type_as(arg4, mold4)) error stop 58_4
    if(same_type_as(arg4, mold5)) error stop 59_4
    if(same_type_as(arg4, mold6)) error stop 60_4
    if(same_type_as(arg5, mold1)) error stop 61_4
    if(same_type_as(arg5, mold2)) error stop 62_4
    if(same_type_as(arg5, mold3)) error stop 63_4
    if(same_type_as(arg5, mold4)) error stop 64_4
    if(.NOT. same_type_as(arg5, mold5)) error stop 65_4
    if(same_type_as(arg5, mold6)) error stop 66_4
    if(same_type_as(arg6, mold1)) error stop 67_4
    if(same_type_as(arg6, mold2)) error stop 68_4
    if(same_type_as(arg6, mold3)) error stop 69_4
    if(same_type_as(arg6, mold4)) error stop 70_4
    if(same_type_as(arg6, mold5)) error stop 71_4
    if(.NOT. same_type_as(arg6, mold6)) error stop 72_4
end
