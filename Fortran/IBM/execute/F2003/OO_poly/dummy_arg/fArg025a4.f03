! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (intrinsic elemental
!                               procedure can be used as actual argument)
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

program fArg025a4

    intrinsic abs   !<-- this is an elemental func

    call test1 (abs, (/-1.0, -2.0, -.5/))

    contains

    subroutine test1 (func, r)
        procedure (real*4) func
        real*4, intent(in) :: r (:)

        do i = 1, size(r)
            print *, func(r(i))
        end do
    end subroutine
end