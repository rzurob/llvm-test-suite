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
!*  DESCRIPTION                : assignment: with some deferred binding and some specific binding
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

   type, abstract :: base
      integer(4) :: i = -999
      contains
         procedure(dintf), deferred, pass :: assgn
         procedure, pass :: assgnint
         generic :: assignment(=) => assgn, assgnint
   end type

   type, extends(base) :: child
      contains
         procedure, pass :: assgn
   end type

   interface
      subroutine dintf (a, b)
         import base
         class(base), intent(out) :: a
         class(base), intent(in) :: b
      end subroutine
   end interface

   contains

   subroutine assgn ( a, b)
      class(child), intent(out) :: a
      class(base), intent(in) :: b

      a%i = b%i

      print *, 'assgn'

   end subroutine

   subroutine assgnint ( a, b)
      class(base), intent(out) :: a
      integer, intent(in) :: b

      a%i = b

      print *, 'assgnint'

   end subroutine

end module

program genericAssignmentDeferred002
   use m

   class(base), pointer :: b1
   type(child) :: c1

   allocate ( b1, source = child ( 100 ) )

   c1 = b1
   print *, c1

   c1 = 300
   print *, c1

   b1 = c1
   print *, b1%i

   b1 = 5000 + 4000 - 3000 - 300 - 700 + 5000
   print *, b1%i

end program
