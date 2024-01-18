!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With no class hierarchy
!*                                 - distinguishable by position, but not by arg with names
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

module genericName

   type b1
      integer :: i
      contains
         procedure, nopass :: fourargs1
         procedure, nopass :: fourargs2
         generic :: fourargs => fourargs1, fourargs2
   end type

   contains

      subroutine fourargs1(w,x,y,z)
         real :: w, y
         integer :: x, z

         print *, 'fourargs1'

      end subroutine

      subroutine fourargs2(x,w,z,y)
         real :: x, y
         integer :: w, z

         print *, 'fourargs2'

      end subroutine

end module

program genericAmbiguityTypeBound025d
end program
