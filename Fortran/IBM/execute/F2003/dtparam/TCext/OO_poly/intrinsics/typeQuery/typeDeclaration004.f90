! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration004.f
! opt variations: -qck -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration004.f
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
!*    A   : unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    MOLD: non polymorphic and declared type is extensible.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2)    ! (20,4,10)
        integer, len  :: n2
        character(n2) :: c
    end type
end module

program typeDeclaration004
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2
    type(Base(20,4)) :: mold

    if(extends_type_of(arg1, mold)) error stop 1_4
    if(extends_type_of(arg2, mold)) error stop 2_4
    if(same_type_as(arg1, mold)) error stop 3_4
    if(same_type_as(arg2, mold)) error stop 4_4

    allocate(Child(20,4,10)::arg1)
    if(.NOT. extends_type_of(arg1, mold)) error stop 5_4
    if(same_type_as(arg1, mold)) error stop 6_4
    deallocate(arg1)
    allocate(Base(20,4)::arg1)
    if(.NOT. extends_type_of(arg1, mold)) error stop 7_4
    if(.NOT. same_type_as(arg1, mold)) error stop 8_4
    deallocate(arg1)
    if(extends_type_of(arg1, mold)) error stop 9_4
    if(same_type_as(arg1, mold)) error stop 10_4
end
