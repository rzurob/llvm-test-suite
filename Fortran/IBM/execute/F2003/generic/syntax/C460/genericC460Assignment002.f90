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
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C460: specific-binding exist in parent type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer :: i = -999
      contains
         procedure, pass :: myassgn
   end type

   type, extends(base) :: emptychild
      contains
         procedure, pass :: myassgn => childassgn
   end type

   type, extends(emptychild) :: gen3
      integer :: j = -999
      contains
         generic :: assignment( = ) => myassgn
   end type

   contains

      subroutine myassgn(a, b)
         class(base), intent(out) :: a
         class(base), intent(in)  :: b
         a%i = b%i

         print *, 'baseassgn'

      end subroutine

      subroutine childassgn(a, b)
         class(emptychild), intent(out) :: a
         class(base), intent(in)  :: b

         a%i = b%i

         print *, 'emptychildassgn'

      end subroutine

end module

program genericC460Assignment002
   use m

   type(gen3) :: g1, g2

   g1 = gen3(10, 20)
   g2 = gen3(30, 40)

   g1 = g2
   
   print *, g1%i, g1%j

end program
