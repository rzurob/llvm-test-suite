! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration001.f
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
!*    MOLD: unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    A   : non polymorphic and declared type is extensible.
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

    type ZeroBase
    end type

    type, extends(ZeroBase) :: ZeroChild
    end type
end module

program typeDeclaration001
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2
    type(Child) :: c1
    type(ZeroChild) :: z1

    !----- non zero-size derived type

    if(.NOT. extends_type_of(c1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(c1, mold2)) error stop 2_4

    if(same_type_as(c1, mold1)) error stop 3_4
    if(same_type_as(c1, mold2)) error stop 4_4

    allocate(Base::mold1)
    if(.NOT. extends_type_of(c1, mold1)) error stop 5_4
    if(same_type_as(c1, mold1)) error stop 6_4

    deallocate(mold1)
    allocate(Child::mold1)
    if(.NOT. extends_type_of(c1, mold1)) error stop 7_4
    if(.NOT. same_type_as(c1, mold1)) error stop 8_4

    !----- zero-size derived type

    deallocate(mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(z1, mold2)) error stop 10_4

    if(same_type_as(z1, mold1)) error stop 11_4
    if(same_type_as(z1, mold2)) error stop 12_4

    allocate(ZeroBase::mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 13_4
    if(same_type_as(z1, mold1)) error stop 14_4

    deallocate(mold1)
    allocate(ZeroChild::mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 15_4
    if(.NOT. same_type_as(z1, mold1)) error stop 16_4
end
