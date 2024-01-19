! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               Diagnostic: passed-object dummy-arg in procedure
!                               call; mis-match for the KIND type parameter
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        integer (k) id
        procedure(updateInt), pass, pointer :: p
    end type

    abstract interface
        subroutine updateInt (b, val)
        import
            class(base), intent(inout) :: b
            integer, intent(in) :: val
        end subroutine
    end interface
end module

program dtparamPassObj001d1
use m
    type(base(8)) b1

    procedure(updateInt) updateBase

    b1%p => updateBase

    call b1%p(100)      !<-- illegal
end

subroutine updateBase (b, val)
use m
    class(base), intent(inout) :: b
    integer, intent(in) :: val

    b%id = val
end subroutine
