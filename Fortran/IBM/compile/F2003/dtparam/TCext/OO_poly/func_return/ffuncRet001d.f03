! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet001d.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    interface
        class (base(4)) function genBase (b)       !<-- must be POINTER/ALLOCATABLE
            import base
            class (base(4)), intent(in) :: b
        end function

        class (base(4)) function genBaseArray (b)
            import base
            class (base(4)), intent(in) :: b(:)

            dimension genBaseArray(size(b))     !<-- illegal
        end function
    end interface
end module

program ffuncRet001d
end