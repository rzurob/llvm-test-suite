!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with polymorphic inner type within types
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

   type innerbase
      integer :: i
      contains
         generic, private :: operator(+) => inneradd
         procedure :: inneradd
   end type

   type, extends(innerbase) :: innerchild
      integer :: j
      contains
         procedure :: inneradd => innercadd
   end type

   type base
      class(innerbase), allocatable :: b1
      contains
         generic :: operator(+) => add
         procedure :: add
   end type

   contains

   class(innerbase) function inneradd ( a, b )
      intent(in) :: a, b
      class(innerbase) :: a, b
      allocatable :: inneradd

      allocate (inneradd)

      inneradd%i = a%i + b%i

   end function

   type(base) function add ( a, b )
      intent(in) :: a, b
      class(base) :: a, b
      if ( allocated ( add%b1 ) ) deallocate ( add%b1 )
      allocate ( add%b1, source = a%b1 + b%b1 )
   end function

   class(innerbase) function innercadd ( a, b )
      intent(in) :: a, b
      class(innerchild) :: a
      class(innerbase) b

      allocatable :: innercadd

      select type ( b )
         type is ( innerchild )
            allocate ( innercadd, source = innerchild ( innerbase = ( a%innerbase + b%innerbase ), j = a%j + b%j ) )
         class default
            error stop 10_4
      end select

   end function

end module

program genericOperatorScalar009
   use m

   class(base), allocatable :: b1
   class(base), allocatable :: b2, b3

   allocate ( b1, source = base ( innerbase (10) ) )
   allocate ( b2, source = b1 + base ( innerbase (10) ) )

   allocate ( b3, source = b1 + b2 )

   if ( b1%b1%i /= 10 ) error stop 1_4
   if ( b2%b1%i /= 20 ) error stop 2_4
   if ( b3%b1%i /= 30 ) error stop 3_4

   deallocate ( b1, b2, b3 )

   allocate ( b1, source = base ( innerchild (10,100) ) )
   allocate ( b2, source = b1 + base ( innerchild (20,200) ) )

   allocate ( b3, source = b1 + b2 )

   select type ( g=> b1%b1 )
      type is ( innerchild )
         if ( ( g%i /= 10 ) .or. ( g%j /= 100 ) )  error stop 4_4
      class default
         error stop 5_4
   end select

   select type ( g=>b2%b1 )
      type is ( innerchild )
         if ( (g%i /= 30 ) .or. (g%j /= 300 ) )  error stop 6_4
      class default
         error stop 7_4
   end select

   select type ( g=> b3%b1 )
      type is ( innerchild )
         if ( ( g%i /= 40 ) .or. ( g%j /= 400 ) )  error stop 8_4
      class default
         error stop 9_4
   end select

end program
