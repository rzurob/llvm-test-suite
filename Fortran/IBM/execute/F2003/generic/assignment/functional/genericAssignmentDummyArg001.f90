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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: non-poly dummy arguments being the operand
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
      integer(4) :: i
      contains
         procedure, pass :: bamt
         generic :: assignment(=) => bamt
   end type

   interface
      subroutine bamt ( a, b )
         import base
         class(base), intent(out) :: a
         type(base), intent(in) :: b
      end subroutine
   end interface

end module

program genericAssignmentDummyArg001
   use m

   type(base) :: b1, b2

   b1 = base(100)
   print *, b1%i

   b2 = base(200)
   print *, b2%i

   call assignment( b1, b2 )
   print *, b1%i
   
   call assignment ( b2, base(200) )
   print *, b2%i

   contains

      subroutine assignment(a, b)
         type(base), intent(inout) :: a
         type(base), intent(in)  :: b

         print *, 'assignment'
         a = b

      end subroutine

end program

subroutine bamt ( a, b )
   use m, only: base
   class(base), intent(out) :: a
   type(base), intent(in) :: b

   a%i = b%i + 1

   print *, 'bamt'

end subroutine
