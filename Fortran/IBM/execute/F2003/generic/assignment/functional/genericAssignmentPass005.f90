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
!*  DESCRIPTION                : assignment: pass-obj specified polymorphic assignment to different derived types
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
         procedure, pass(b) :: base1_base
         generic :: assignment(=) => base1_base
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
      contains
         procedure, pass(b) :: base1_base => base1_child
   end type

   type base1
      integer(8) :: k = -999
      contains
         procedure, pass(b) :: base_base1
         generic :: assignment(=) => base_base1
   end type

   type, extends(base1) :: child1
      integer(8) :: l = -999
      contains
         procedure, pass(b) :: base_base1 => base_child1
   end type

   contains

   subroutine base1_base ( a, b )
      class(base1), intent(out) :: a
      class(base), intent(in)   :: b

      a%k = INT(b%i,8)

      print *,'base1_base'

   end subroutine

   subroutine base_base1 ( a, b )
      class(base), intent(out) :: a
      class(base1), intent(in) :: b

      a%i = INT(b%k,4)

      print *,'base_base1'

   end subroutine

   subroutine base1_child ( a, b )
      class(base1), intent(out) :: a
      class(child), intent(in)   :: b

      a%k = INT(b%i,8)

      select type ( a )
         type is ( child1 )
            a%l = INT(b%j,8)
      end select

      print *,'base1_child'

   end subroutine

   subroutine base_child1 ( a, b )
      class(base), intent(out) :: a
      class(child1), intent(in) :: b

      a%i = INT(b%k,4)

      select type ( a )
         type is ( child )
            a%j = b%l
      end select

      print *,'base_child1'

   end subroutine


end module

program genericAssignmentPass005
   use m

   class(base), allocatable  :: b1
   class(base1), allocatable :: bb1

   allocate ( b1, bb1 )

   b1 = base1(1000)
   print *, b1%i

   b1 = child1(500, 1000)
   print *, b1%i

   bb1 = base(10)
   print *, bb1%k

   bb1 = child(5, 10)
   print *, bb1%k

   bb1 = b1
   print *, bb1%k

   bb1%k = 50

   b1 = bb1
   print *, b1%i

   deallocate ( b1, bb1 )

   allocate ( child :: b1 )
   allocate ( child1 :: bb1 )

   b1 = base1(1)
   print *, b1%i

   b1 = child1(5, 10)
   print *, b1%i

   bb1 = base(1)
   print *, bb1%k

   bb1 = child(5, 10)
   print *, bb1%k

   bb1 = b1
   print *, bb1%k

   bb1%k = 50

   b1 = bb1
   print *, b1%i

end program
