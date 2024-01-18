!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: UD operator subroutine is a pure function
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
         procedure, pass :: mybadd
         generic :: operator(*) => mybadd
   end type

   contains

   type(base) pure function mybadd ( a, b )
      class(base), intent(in) :: a
      integer, intent(in) :: b

      mybadd%x = a%x * b

   end function

end module

program genericOperatorPure001
   use m

   type(base) :: b1 = base(100)
   type(base) :: b2 = base(200)
   type(base) :: b3

   b3 = b1 * 10
   print *, b3

   b2 = b3 * 5
   print *, b2

   b1 = base(20) * 7
   print *, b1

end program
