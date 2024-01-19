
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: a final-
!*                               subroutine-name shall be nonoptional
!*                               and shall be a nonpointer, nonallocatable
!*                               nonpolymorphic variable of the derived
!*                               type being defined.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final :: finalizeBase !* expect an error message here
    end type

    type, extends(base) :: child
    contains
       final :: finalizeChild !* FE supresses error message here as the parent is already in error
    end type

    contains
    subroutine finalizeBase (b1)
       type(base), pointer :: b1
       print *, 'finalizeBase'
    end subroutine
    subroutine finalizeChild (b1)
       type(base), intent(inout) :: b1
       print *, 'finalizeChild'
    end subroutine
end module
end
