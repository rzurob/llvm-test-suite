!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with array components
!*                                         using intrinsic operator inside defined operator function with allocatable components
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
      integer, allocatable:: i(:)
      contains
         procedure, pass :: add
         generic :: operator ( + ) => add
   end type

   contains

   type(base) function add ( a, b )
      class(base), intent(in) :: a, b

      if ( ( .not. allocated (a%i)) .or. ( .not. allocated (b%i)) ) error stop 1_4
      if ( size(b%i) .lt. size (a%i) ) error stop 2_4

      allocate (add%i (size(a%i)))
      add%i = a%i + b%i(1:size(a%i))

   end function

end module

program genericOperatorArrayComp001a2
   use m

   type (base) :: b1
   type (base), allocatable :: b2

   b1 = base ( (/ 1,2,3,4/) )

   allocate ( b2 )
   b2 = base ( (/ 5, 6, 7, 8, 9, 10 /) )

   associate ( g => b2 + b2 + b2 )
      print *, g%i
   end associate

   associate ( g => b1 + b1 + b1 )
      print *, g%i
   end associate

   associate ( g => b1 + b2 + base((/5,6,7,8,9,10,11/)) )
      print *, g%i
   end associate

end program
