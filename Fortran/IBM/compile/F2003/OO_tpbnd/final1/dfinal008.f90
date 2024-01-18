!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               the dummy argument shall not be
!*                               intent(out).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains

!* expect error message 1514-598 here
        final  :: finalizeBase
    end type

    contains
    subroutine finalizeBase (b1)
       type(base), intent(out) :: b1
       print *, 'finalizeBase'
    end subroutine
end module
end
