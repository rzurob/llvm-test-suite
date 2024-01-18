! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return (poly-function results must
!                               have POINTER/ALLOCATABLE attribute)
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
        function genBase (b)
            import base
            class (base), intent(in) :: b
            class (base) genBase
        end function

        function genBaseArray (b)
            import base
            class (base), intent(in) :: b(:)
            class (base) genBaseArray(size(b))
        end function
    end interface
end module

program ffuncRet001d1
end
