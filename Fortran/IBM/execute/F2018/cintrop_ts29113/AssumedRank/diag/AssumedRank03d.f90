! *********************************************************************
!* ===================================================================
!*
!* DATE                         : August  25, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : An assumed-type actual argument that
!*                                corresponds to an assumed-rank dummy
!*                                argument shall be assumed-shape or assumed-rank. (C407c)
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub(a, b, c)
type(*) :: a, b(:), c(..)
call foo(a)
call foo(b)
call foo(c)
contains
    subroutine foo(t)
        type(*) :: t(..)
    end subroutine foo
end subroutine sub
