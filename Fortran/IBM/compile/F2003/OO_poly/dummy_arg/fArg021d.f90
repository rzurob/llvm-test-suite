! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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
    contains

    subroutine test1 (x)
        class (*), intent(inout) :: x(:)
    end subroutine

    subroutine test2 (x)
        class (*), intent(out) :: x(:)
    end subroutine
end module

program fArg021d
use m
    integer*4 i1 (10)

    class (*), pointer :: x1 (:)

    i1 = 1

    allocate (integer*4 :: x1(20))

    call test1 ((/1,2,3/))  !<-- this is illegal

    call test2 (x1(i1))     !<-- this is illegal
end
