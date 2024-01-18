! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor008.f
! opt variations: -qck -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstructor008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    A   : is specified using structure constructor with extensible
!*          non abstract type.
!*    MOLD: polymorphic but not unlimited polymorphic, declared type
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

program structureConstructor008
use m
    class(Base(4)), pointer :: mold1 => null()
    class(AbstractParent(4)), allocatable :: mold2

    if(.NOT. extends_type_of(Base(4)(1), mold1)) error stop 1_4
    if(.NOT. extends_type_of(Child(4,10)(1, "abc"), mold2)) error stop 2_4
    if(.NOT. same_type_as(Base(4)(1), mold1)) error stop 3_4
    if(same_type_as(Child(4,10)(1, "abc"), mold2)) error stop 4_4

    allocate(Child(4,10)::mold1)
    allocate(Base(4)::mold2)

    if(extends_type_of(Base(4)(1), mold1)) error stop 5_4
    if(.NOT. extends_type_of(Child(4,10)(1, "abc"), mold2)) error stop 6_4
    if(.NOT. same_type_as(Child(4,10)(1, "abc"), mold1)) error stop 7_4
    if(.NOT. same_type_as(Base(4)(1), mold2)) error stop 8_4
end
