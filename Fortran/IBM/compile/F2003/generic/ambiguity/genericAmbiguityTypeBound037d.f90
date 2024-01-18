!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : ambiguous by using argument names
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
         procedure, nopass :: twoargs1
         generic :: twoargs => twoargs1
   end type

   type, extends(b1) :: c1
      contains
         procedure, nopass :: twoargs2
         generic :: twoargs => twoargs2
   end type

   contains

      subroutine twoargs1(y, z)
         type(b1) :: y
         type(c1) :: z

         print *, 'twoargs1'

      end subroutine

      subroutine twoargs2(z, y)
         type(b1) :: y
         type(c1) :: z
         
         print *, 'twoargs2'

      end subroutine

end module

program genericAmbiguityTypeBound037d
end program
