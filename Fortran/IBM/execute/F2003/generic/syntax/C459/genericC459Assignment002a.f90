!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( = )
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with private attribute
!*                                     intrinsic assignment should be used instead of generic assignment
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

   type :: base
      character(3) :: c ='xxx'
      contains
         generic, private :: assignment(=) => typetotype
         procedure, pass, private :: typetotype => btob
   end type

   contains

   subroutine btob ( a , b )
      class(base) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c
      print *, 'bad'
   end subroutine

end module

program genericC459Assignment002
   use m

   type(base) :: b1, b2

   b1 = base ('xxx')
   b2 = base ('ibm')

   b1 = b2

   if ( b1%c /= 'ibm' ) error stop 1_4

end program
