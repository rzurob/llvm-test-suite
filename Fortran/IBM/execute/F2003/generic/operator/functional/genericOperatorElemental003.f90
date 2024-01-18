!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: with elemental function with unary operator
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
      integer :: x = -999
      contains
         generic :: operator(.not.) => not
         procedure :: not
   end type

   contains

      elemental type(base) function not ( a )
         class(base), intent(in) :: a

         not%x = -1 * a%x

      end function

end module

program genericOperatorElemental003
   use m

   type(base) :: b1, b2(3)
   class(base), allocatable :: b3(:), b4(:)

   b1 = base(10)

   b2 = (/ ( base(i), i = 100, 300, 100 ) /)
   allocate ( b3(size(b2)), source = .not. b2  )

   print *, b1
   print *, b2
   print *, b3%x

   b1 = .not. b1
   b2 = .not. ( .not. (.not. b2 ) )

   allocate ( b4(5), source = (/ .not. b3, .not. b1, .not. b2(2) /) )

   print *, b1
   print *, b2
   print *, b4%x

end program
