!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: different type in the second argument in the same type
!*                                         such that the 2nd arg types are parent and extended types
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
         procedure :: withbase
         procedure :: withchild
         generic :: operator(-) => withbase, withchild
   end type

   type, extends(base) :: child
      integer :: j
   end type

   contains

      class(base) function withbase ( a, b )
         allocatable :: withbase
         class(base), intent(in) :: a
         type(base) , intent(in) :: b
         allocate ( withbase , source = base ( a%i - b%i ) )

      end function

      class(base) function withchild ( a, b )
         allocatable :: withchild
         class(base), intent(in) :: a
         type(child) , intent(in) :: b

         allocate ( withchild , source = child ( base = a - b%base, j = -b%j ) )

      end function

end module

program genericOperatorScalar015
   use m

   type(base) :: b1 = base(100)
   type(base) :: b2
   class(base), pointer :: b3

   type(child) :: c1 = child ( 300, 400 )

   allocate ( b3 , source = base (200) )

   b2 = b1 - b3
   if ( b2%i /= -100 ) error stop 1_4

   b2 = b3 - b1
   if ( b2%i /= 100 )  error stop 2_4

   associate ( g => b1 - c1 )
      select type ( g )
         type is ( child )
            if ( ( g%i /= -200 ) .or. ( g%j /= -400 ) ) error stop 3_4
         class default
            error stop 4_4
      end select
   end associate

   select type ( g => c1 - b1 )
      type is ( base )
         if ( g%i /= 200 ) error stop 5_4
      class default
         error stop 6_4
   end select

   select type ( g => c1 - c1 )
      type is ( child )
         if ( ( g%i /= 0 ) .or. (g%j /= -400 ) ) error stop 7_4
      class default
         error stop 8_4
   end select

end program
