!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors with class hierarchy
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
      integer i
      contains
         procedure, pass, private :: mulwint
         procedure, pass, private :: mulwtype
         generic :: operator(*) => mulwint, mulwtype
   end type

  type, extends(base) :: child
      integer j
      contains
         procedure, pass, private :: mulwint => mulchildwint
         procedure, pass, private :: mulwtype => mulchildwtype
   end type

   contains

   function mulwint ( a, b )
      class(base), intent(in) :: a
      integer, intent(in) :: b
      class(base), allocatable :: mulwint

      allocate ( mulwint )

      mulwint%i = a%i * b

   end function

   function mulwtype ( a, b )
      class(base), intent(in) :: a, b
      class(base), allocatable :: mulwtype
      allocate ( mulwtype )
      mulwtype%i = a%i * b%i

   end function

   function mulchildwint ( a, b )
      class(child), intent(in) :: a
      integer, intent(in) :: b
      class(base), allocatable :: mulchildwint

      allocate ( mulchildwint , source = child ( base = a%base * b, j = a%j * b) )

   end function

   function mulchildwtype ( a, b )
      class(child), intent(in) :: a
      class(base) , intent(in) :: b
      class(base), allocatable :: mulchildwtype

      select type ( g => b )
         type is (base)
            allocate ( mulchildwtype , source = child ( base = a%base * g, j = a%j) )
         type is (child)
            allocate ( mulchildwtype , source = child ( base = a%base * g%base, j = a%j * g%j) )
      end select

   end function

end module


program genericOperatorStructConstr002
   use m

   type (base) :: b1
   type(child) :: c1

   class(base), allocatable :: b2


   b1 = base(10) * 4
   if ( b1%i /= 40 ) error stop 1_4

   b1 = base(20) * base(-20)
   if ( b1%i /= -400 ) error stop 2_4

   c1%base = base(-2) * (-10)
   if ( c1%i /= 20 ) error stop 3_4

   allocate ( b2, source = ( child(10,20) * 3 ) )

   select type ( b2 )
      type is (child)
         if ( ( b2%i /= 30 ) .or. ( b2%j /= 60 ) ) error stop 4_4
      class default
         error stop 5_4
   end select

   deallocate ( b2)
   allocate ( b2, source = ( child (5,8) * base (10) ) )

   select type ( b2 )
      type is (child)
         if ( ( b2%i /= 50 ) .or. ( b2%j /= 8 ) ) error stop 6_4
      class default
         error stop 7_4
   end select

   deallocate ( b2)
   allocate ( b2, source = ( child (2,4) * child (6,8) ) )

   select type ( b2 )
      type is (child)
         if ( ( b2%i /= 12 ) .or. ( b2%j /= 32 ) ) error stop 8_4
      class default
         error stop 9_4
   end select

end
