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

    type, extends(base) :: child
    contains
       final :: finalizeChild
    end type

    type(child), allocatable :: t_c1(:)

    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child), intent(inout) :: b1(:)
        print *, 'finalizeChild'
    end subroutine

end module
    use m
    type(child), pointer :: t_c2(:)
    allocate(t_c1(1), t_c2(2))
    deallocate(t_c1, t_c2)

end
