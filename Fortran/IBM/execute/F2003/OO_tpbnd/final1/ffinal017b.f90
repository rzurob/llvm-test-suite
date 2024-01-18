!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: final
!*                               subroutines are not inherited
!*                               through type extension.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final :: finalizeBase
    end type

    type, extends(base) :: parent
    end type

    type, extends(parent) :: child
    contains
       final :: finalizeChild
    end type

    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child), intent(inout) :: b1
        print *, 'finalizeChild'
    end subroutine

end module
    call sub()

end

subroutine sub()
    use m
    type(child), pointer :: t_c1
    allocate(t_c1)
    deallocate(t_c1)
end subroutine
