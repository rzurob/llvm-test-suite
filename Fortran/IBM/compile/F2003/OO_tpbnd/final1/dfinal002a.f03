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
       final :: finalizeChild ! FE supresses this error message as it may cause ICE
    end type

    contains
    subroutine finalizeBase (b1)
       class(base) :: b1
       print *, 'finalizeBase'
    end subroutine
    subroutine finalizeChild (b1)
       class(child), intent(inout) :: b1
       print *, 'finalizeChild'
    end subroutine
end module
end