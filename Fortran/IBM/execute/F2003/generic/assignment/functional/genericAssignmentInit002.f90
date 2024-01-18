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
!*  DESCRIPTION                : assignment: initialization expression is not the same as assignment
!*                                           with class hierarchy (generic or intrinsic)
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
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child1
      integer :: j = -999
   end type

   type, extends(base) :: child2
      integer :: k = -999
      contains
         procedure, pass :: bassgn => cassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i + 1
         select type ( a )
            type is ( child1 )
               select type ( b )
                  type is ( child1 )
                     a%j = b%j + 1
                  type is ( child2 )
                     a%j = b%k + 1
               end select
         end select

         print *, 'bassgn'

      end subroutine

      subroutine cassgn ( a, b )
         class(child2), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i + 1
         select type ( b )
            type is ( child2 )
               a%k = b%k + 1
            type is ( child1 )
               a%k = b%j + 1
         end select
         print *, 'cassgn'

      end subroutine

end module


program genericAssignmentInit002
   use m

   type(base) :: b1 = base(100)
   type(child1) :: c1 = child1(200, 300)
   type(child2) :: c2 = child2(400, 500)

   print *, b1
   print *, c1
   print *, c2

   b1 = c1
   c1 = c2
   c2 = b1

   print *, b1
   print *, c1
   print *, c2

end program
