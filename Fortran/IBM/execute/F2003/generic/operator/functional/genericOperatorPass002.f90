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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Binary Operator: with pass attribute (+) with overridding binding
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
         procedure, pass    :: base_base_int
         procedure, pass(b) :: base_int_base
         procedure, pass(b) :: base_base_base
         generic :: operator(+) => base_base_int, base_int_base, base_base_base
   end type

   type, extends(base) :: child
      contains
         procedure, pass    :: base_base_int => base_child_int
         procedure, pass(b) :: base_int_base => base_int_child
         procedure, pass(b) :: base_base_base => base_base_child
   end type

   contains

   function base_base_base ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in) :: b

      class(base), allocatable :: base_base_base

      allocate ( base_base_base, source = b )

      base_base_base%i = a%i + base_base_base%i
      print *, 'base_base_base'

   end function

   function base_int_base ( a, b )
      integer, intent(in) :: a
      class(base), intent(in)  :: b

      class(base), allocatable :: base_int_base

      allocate ( base_int_base, source = b )

      base_int_base%i = a + base_int_base%i
      print *, 'base_int_base'

   end function

   function base_base_int ( a, b )
      class(base), intent(in) :: a
      integer, intent(in)  :: b

      class(base), allocatable :: base_base_int
      allocate ( base_base_int, source = a )

      base_base_int%i = base_base_int%i + b
      print *, 'base_base_int'

   end function

   function base_base_child ( a, b )
      class(base), intent(in) :: a
      class(child), intent(in) :: b

      class(base), allocatable :: base_base_child
      allocate ( base_base_child , source = b )

      base_base_child%i = a%i + base_base_child%i
      print *, 'base_base_child'

   end function

   function base_int_child ( a, b )
      integer, intent(in) :: a
      class(child), intent(in)  :: b

      class(base), allocatable :: base_int_child
      allocate ( base_int_child, source = b )

      base_int_child%i = a + base_int_child%i
      print *, 'base_int_child'

   end function

   function base_child_int ( a, b )
      class(child), intent(in) :: a
      integer, intent(in)  :: b

      class(base), allocatable :: base_child_int
      allocate ( base_child_int, source = a )

      base_child_int%i = base_child_int%i + b
      print *, 'base_child_int'

   end function

end module

program genericOperatorPass002
   use m

   class(base), allocatable :: b1, b2
   class(base), pointer :: c1

   allocate ( b1, b2, c1 )

   select type ( b1 )
      type is ( base )
         b1 = base(10) + 20 + child(30) + 40 + base(50)
         print *, b1
   end select

   select type ( b2 )
      type is ( base )
         b2 = base(10) + b1 + child(30) + b1 + base(50)
         print *, b2
   end select

   select type ( c1 )
      type is (base)
         c1 = b1 + 20 + b2 + base(10)
         print *,c1%i
   end select

   deallocate ( b1, c1, b2 )

   allocate ( b2, source= child(100) )

   associate ( g => child(10) + 10 + base(20) + 20 + b2 )
      select type ( g )
         class is ( base )
            allocate (  b1, source = g )
            print *, b1%i
      end select
   end associate

   allocate ( c1 , source = b1 + 50 + b2 + base(10) + child(20) + 100 )
   print *, c1%i

end program
