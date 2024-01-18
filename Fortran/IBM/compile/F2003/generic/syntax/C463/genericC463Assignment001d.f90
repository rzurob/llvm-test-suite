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
!*  DESCRIPTION                : C463: define as function
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
      integer x,y
      contains
         procedure, pass :: mybassgn => b2b
         generic :: assignment(=) => mybassgn
   end type

   interface
      function b2b(a,b)
         import base
         class(base), intent(out) :: a
         class(base), intent(in) :: b
         type(base) :: b2b
      end function
   end interface

   type base2
      integer i
      contains
         procedure, pass :: b2b2
         generic :: assignment(=) => b2b2
   end type

   contains

   function b2b2(a,b)

      class(base2), intent(out) :: a
      class(base2), intent(in) :: b
      type(base2) :: b2b2

      b2b2%i = a%i

   end function

end module

end

function b2b(a,b)
   use m, only: base
   class(base), intent(out) :: a
   class(base), intent(in) :: b
   type(base) :: b2b

   b2b%x = a%x
   b2b%y = b%y

end function
