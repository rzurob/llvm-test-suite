! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (deallocate of a pointer whose target
!                               is not created by allocate causes an error
!                               condition)
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
use ISO_C_BINDING

    contains

    subroutine test1 (a, b)
        class (*), target :: a(:), b(3)
        type seq1
            sequence
            integer(4) i, j
        end type

        type, bind(c) :: bType
            integer(c_int) i,j
        end type

        integer err(2)

        type (seq1), pointer :: s_ptr(:)
        type (bType), pointer :: b_ptr(:)

        err = 0
        s_ptr => a
        b_ptr => b

        deallocate (s_ptr, stat=err(1))
        deallocate (b_ptr, stat=err(2))

        print *, err
    end subroutine
end module

program falloc027a1
use m
use ISO_C_BINDING
    type seq1
        sequence
        integer(4) i, j
    end type

    type, bind(c) :: bType
        integer(c_int) i,j
    end type

    type (seq1), target :: s11(10)
    type (bType), target :: b11(20)

    call test1 (s11, b11)
end
