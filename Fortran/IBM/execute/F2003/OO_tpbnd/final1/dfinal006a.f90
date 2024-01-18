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
!*                               dup "final" keyword
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        procedure, nopass :: final
        final  :: final
    end type

    contains
    subroutine final (b1)
       type(base), intent(in) :: b1
       print *, 'finalizeBase'
    end subroutine
end module

   use m
   type(base) :: dt

   call dt%final(dt)

   print *, 'second test'

   call sub (dt)

   print *, 'end'
end

subroutine sub(arg1)
   use m
   type(base), intent(out) :: arg1
end subroutine
