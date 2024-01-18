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
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: pass-obj specified with array dummy arg and elemental
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
      integer(4) :: i = -999
      contains
         procedure, pass(b) :: base_base
         generic :: assignment(=) => base_base
   end type

   type, extends(base) :: child1
      integer(4) :: j = -999
      contains
         procedure, pass(b) :: base_base => base_child1
         generic :: assignment(=) => base_base
   end type

   type, extends(base) :: child2
      integer(4) :: k = -999
      contains
         procedure, pass(b) :: base_base => base_child2
   end type

   contains

   subroutine base_base ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in)  :: b

      a%i = b%i

      print *,'base_base'

   end subroutine

   subroutine base_child1 ( a, b )
      class(base), intent(out) :: a
      class(child1), intent(in)  :: b

      a%i = b%i

      select type ( a )
         type is ( child1 )
            a%j = b%j
         type is ( child2 )
            a%k = b%j
      end select

      print *,'base_child1'

   end subroutine

   subroutine base_child2 ( a, b )
      class(base), intent(out) :: a
      class(child2), intent(in)  :: b

      a%i = b%i

      select type ( a )
         type is ( child1 )
            a%j = b%k
         type is ( child2 )
            a%k = b%k
      end select

      print *,'base_child2'

   end subroutine

end module

program genericAssignmentPass012
   use m

   class(base), allocatable :: b1, b2
   class(child1), pointer :: c1
   class(child2), pointer :: c2

   allocate ( b1, b2, c1, c2 )

   b1 = base(10)
   print *, b1%i

   b2 = child1(20,200)
   print *, b2%i

   b1 = child2(30,300)
   print *, b1%i

   c1 = child2(40,400)
   print *, c1%i, c1%j

   c2 = child1(40,400)
   print *, c2%i, c2%k

   c2 = b1
   print *, c2%i, c2%k

   c1 = b2
   print *, c1%i, c1%j

   deallocate ( b1, b2 )

   allocate ( child1 :: b1 )
   allocate ( child2 :: b2 )

   b1 = child2(50,500)
   select type ( b1 )
      type is ( child1 )
         print *, b1%i, b1%j
   end select

   b2 = child1(60,600)
   select type ( b2 )
      type is ( child2 )
         print *, b2%i, b2%k
   end select

end program
