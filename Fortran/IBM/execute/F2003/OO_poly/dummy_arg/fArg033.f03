! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument associated (rule 1 in section
!                               12.4.1.7: change of dummy-arg's value through
!                               actual arg)
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
        integer*4 :: id
    end type


    class (base), pointer :: b1_m (:)
    type (base), target :: c1 (3)

    contains

    subroutine test2 (b)
        class (base), pointer :: b(:)

        b1_m => c1

        if (.not. associated (b, c1)) error stop 2_4
    end subroutine
end module

program fArg033
use m
    allocate (b1_m(20))

    call test1 (b1_m)


    call test2 (b1_m)

    contains

    subroutine test1 (b)
        class (base), pointer :: b(:)

        deallocate (b1_m)

        if (associated (b)) error stop 1_4
    end subroutine
end
