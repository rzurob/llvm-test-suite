!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: A final
!*                               -subroutine-name shall not be one
!*                               previously specified as a final
!*                               subroutine for that type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final  :: finalizeBase, finalizeBase !* expect an error message here
    end type

    contains
    subroutine finalizeBase (b1)
       type(base) :: b1
       print *, 'finalizeBase'
    end subroutine
end module
end
