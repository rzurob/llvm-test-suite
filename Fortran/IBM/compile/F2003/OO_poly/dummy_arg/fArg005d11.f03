! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (for pointer and
!*                               allocatable dummy-args the ranks of the
!*                               actual-args shall agree with dummy)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    end type

    contains

    subroutine scalarPointer (b)
        class (base), pointer :: b
    end subroutine

    subroutine scalarAllocatable (b)
        class (base), allocatable :: b
    end subroutine

    subroutine rankOnePointer (b)
        class (base), pointer :: b(:)
    end subroutine

    subroutine rankOneAllocatable (b)
        class (base), allocatable :: b(:)
    end subroutine
end module

program fArg005d11
use m
    class (base), pointer :: b1, b2(:), b3(:,:)
    class (base), allocatable :: ba1, ba2(:), ba3(:,:)

    !! each of the following calls will receive an error message

    call scalarPointer (b2)
    call scalarPointer (b3)

    call scalarAllocatable (ba2)
    call scalarAllocatable (ba3)

    call rankOnePointer (b1)
    call rankOnePointer (b3)

    call rankOneAllocatable (ba1)
    call rankOneAllocatable (ba3)
end
