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
!*  DESCRIPTION                : assignment: pass-obj specified with function result in assignment
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
         procedure, pass(b) :: int_base
         generic :: assignment(=) => int_base
         procedure, pass(b) :: base_base
         generic :: assignment(=) => base_base
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
      contains
         procedure, pass(b) :: int_base => int_child
         procedure, pass(b) :: base_base => base_child
   end type

   type, extends(child) :: gen3
      integer(4) :: k = -999
      contains
         procedure, pass(b) :: int_base => int_gen3
         procedure, pass(b) :: base_base => base_gen3
   end type


   contains

   subroutine base_base ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in)  :: b

      a%i = b%i

      print *,'base_base'

   end subroutine

   subroutine base_child ( a, b )
      class(base), intent(out) :: a
      class(child), intent(in)  :: b

      a%i = b%i

      select type ( a )
         class is ( child )
            a%j = b%j
      end select

      print *,'base_child'

   end subroutine

   subroutine base_gen3 ( a, b )
      class(base), intent(out) :: a
      class(gen3), intent(in)  :: b

      a%i = b%i

      select type ( a )
         class is ( child )
            a%j = b%j
         class is ( gen3 )
            a%j = b%j
            a%k = b%k
      end select

      print *,'base_gen3'

   end subroutine

   subroutine int_base ( a, b )
      integer, intent(out) :: a
      class(base), intent(in)  :: b

      a = b%i

      print *,'int_base'

   end subroutine

   subroutine int_child ( a, b )
      integer, intent(out) :: a
      class(child), intent(in)  :: b

      a = b%i + b%j

      print *,'int_child'

   end subroutine

   subroutine int_gen3 ( a, b )
      integer, intent(out) :: a
      class(gen3), intent(in)  :: b

      a = b%i + b%j + b%k

      print *,'int_gen3'

   end subroutine

   class(base) function gen_base_alloc( unallocated )
      class(base), intent(in) :: unallocated

      allocatable :: gen_base_alloc

      select type ( unallocated )
         class is ( base )
            allocate ( gen_base_alloc, source = base(1) )
         class is ( child )
            allocate ( gen_base_alloc, source = child(10,20) )
         class is ( gen3 )
            allocate ( gen_base_alloc, source = gen3(100,200,300) )
         end select

   end function

end module

program genericAssignmentPass014
   use m

   class(base), allocatable :: b1
   class(child), allocatable :: c1
   class(gen3), allocatable :: g1

   integer :: i

   i= gen_base_alloc(b1)
   print *,i

   i= gen_base_alloc(c1)
   print *,i

   i= gen_base_alloc(g1)
   print *,i
   
   allocate ( b1, c1, g1 )
   
   b1 = gen_base_alloc(b1)
   
   c1 = gen_base_alloc(b1)
   
   g1 = gen_base_alloc(b1)
   
   print *, b1%i, c1%i, g1%i
   
   b1 = gen_base_alloc(c1)
   
   c1 = gen_base_alloc(c1)
   
   g1 = gen_base_alloc(c1)
   
   print *, b1%i, c1%i, c1%j, g1%i, g1%j
   
   b1 = gen_base_alloc(g1)
   
   c1 = gen_base_alloc(g1)
   
   g1 = gen_base_alloc(g1)
   
   print *, b1%i, c1%i, c1%j, g1%i, g1%j, g1%k

end program
