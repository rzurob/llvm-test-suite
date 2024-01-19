! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function return (polymorphic function
!                               results must have POINTER/ALLOCATABLE attribute)
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
        integer(4) :: id
    end type

    interface
        class (base) function genBase (b)       !<-- must be POINTER/ALLOCATABLE
            import base
            class (base), intent(in) :: b
        end function

        class (base) function genBaseArray (b)
            import base
            class (base), intent(in) :: b(:)

            dimension genBaseArray(size(b))     !<-- illegal
        end function
    end interface
end module

program ffuncRet001d
end
