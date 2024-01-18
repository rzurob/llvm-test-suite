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

    type dt
       type(base), pointer :: dt_p => null()
    contains
       final :: finalize
    end type

    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'finalizeBase'
    end subroutine

    subroutine finalize (d1)
        type (dt), intent(inout) :: d1
        if(associated(d1%dt_p))    deallocate(d1%dt_p)
    end subroutine
end module

   use m

   call sub()

end

subroutine sub()
    use m
    type(dt) :: dt_c
    allocate(dt_c%dt_p)
end subroutine
