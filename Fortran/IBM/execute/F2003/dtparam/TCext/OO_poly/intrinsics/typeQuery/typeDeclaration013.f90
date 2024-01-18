! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration013.f
! opt variations: -qck -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration013.f
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
!*    MOLD: polymorphic but not unlimited polymorphic. Declared type is
!*          extensible and can be abstract or non-abstract.
!*    A   : unlimited polymorphic but not disassociated pointer or
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child(n1)    ! (4,10)
        integer, len  :: n1
        character(n1) :: c
    end type
end module

program typeDeclaration013
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2

    class(Base(4)), pointer :: mold1 => null()
    class(AbstractParent(4)), allocatable :: mold2

    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg2, mold1)) error stop 3_4
    if(extends_type_of(arg2, mold2)) error stop 4_4

    if(same_type_as(arg1, mold1)) error stop 5_4
    if(same_type_as(arg1, mold2)) error stop 6_4
    if(same_type_as(arg2, mold1)) error stop 7_4
    if(same_type_as(arg2, mold2)) error stop 8_4

    allocate(Child(4,10)::arg1)
    allocate(Base(4)::arg2)

    if(.NOT. extends_type_of(arg1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 10_4
    if(.NOT. extends_type_of(arg2, mold1)) error stop 11_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 12_4

    if(same_type_as(arg1, mold1)) error stop 13_4
    if(same_type_as(arg1, mold2)) error stop 14_4
    if(.NOT. same_type_as(arg2, mold1)) error stop 15_4
    if(same_type_as(arg2, mold2)) error stop 16_4

    allocate(Child(4,10)::mold1)
    allocate(Base(4)::mold2)

    if(.NOT. extends_type_of(arg1, mold1)) error stop 17_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 18_4
    if(extends_type_of(arg2, mold1)) error stop 19_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 20_4

    if(.NOT. same_type_as(arg1, mold1)) error stop 21_4
    if(same_type_as(arg1, mold2)) error stop 22_4
    if(same_type_as(arg2, mold1)) error stop 23_4
    if(.NOT. same_type_as(arg2, mold2)) error stop 24_4
end
