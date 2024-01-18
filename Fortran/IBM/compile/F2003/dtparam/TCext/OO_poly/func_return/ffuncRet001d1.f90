! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet001d1.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    interface
        function genBase (b)
            import base
            class (base(4)), intent(in) :: b
            class (base(4)) genBase
        end function

        function genBaseArray (b)
            import base
            class (base(4)), intent(in) :: b(:)
            class (base(4)) genBaseArray(size(b))
        end function
    end interface
end module

program ffuncRet001d1
end
