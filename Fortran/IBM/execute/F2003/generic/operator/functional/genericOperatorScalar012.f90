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
!*  DESCRIPTION                : Operator: Scalar function return with polymorphic pointer attribute
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
      integer :: i
      contains
         generic :: operator (-) => sub1, sub2
         procedure, private :: sub1
         procedure, private :: sub2
   end type

   contains

   function sub1 (a, b)
      class(base), intent(in) :: a, b

      class(base), pointer :: sub1
      allocate ( sub1, source = base ( a%i - b%i ) )

   end function sub1

   function sub2 (a, b)
      class(base), intent(in) :: a
      integer, intent(in) :: b

      class(base), pointer :: sub2
      allocate ( sub2, source = base ( a%i - b ) )

   end function sub2

end module

module n
   use m, only: base

   type, extends(base) :: child
     integer :: j
      contains
         procedure, private :: sub1 => subc1
         procedure, private :: sub2 => subc2
   end type

   contains

   function subc1 (a, b)
      class(child), intent(in) :: a
      class(base), intent(in)  :: b

      class(base), pointer :: subc1

      select type ( b )
         type is ( base )
            allocate ( subc1, source = child ( a%i - b%i, j = a%j ) )
         type is ( child )
            allocate ( subc1, source = child ( a%i - b%i, j = a%j - b%j ) )
      end select

   end function subc1

   function subc2 (a, b)
      class(child), intent(in) :: a
      integer, intent(in) :: b

      class(base), pointer :: subc2
      allocate ( subc2, source = child ( a%i - b, j = a%j - b ) )

   end function subc2

end module

program genericOperatorScalar012
   use n

   class(base), pointer  :: b1
   class(base), pointer :: b2

   b1 => base (100) - base (90)
   b2 => child (30,40) - b1

   if ( b1%i /= 10 ) error stop 1_4

   select type ( b2 )
      type is ( child )
         if ( ( b2%i /= 20 ) .or. ( b2%j /= 40 ) )  error stop 2_4
      class default
         error stop 3_4
   end select

   b1 => b1 - b1
   if ( b1%i /= 0 ) error stop 3_4

   b1 => base(10) - (-100)
   if ( b1%i /= 110 ) error stop 4_4

   b1 => child ( 200, 400 ) - 150

   select type ( b1 )
      type is ( child )
         if ( ( b1%i /= 50 ) .or. ( b1%j /= 250 ) )  error stop 5_4
      class default
         error stop 6_4
   end select

   b2 => child (30,40) - b1

   select type ( b2 )
      type is ( child )
         if ( ( b2%i /= -20 ) .or. ( b2%j /= -210 ) )  error stop 7_4
      class default
         error stop 8_4
   end select

end program
