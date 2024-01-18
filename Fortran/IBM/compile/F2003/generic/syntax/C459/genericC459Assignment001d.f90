!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( = )
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within the same derived type
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
      character(3) :: c
      contains
         generic, private :: assignment(=) => typetotype
         procedure, pass, private :: typetotype => btob

         generic :: assignment(=) => chartotype, typetochar
         procedure, pass, private :: chartotype => ctob
         procedure, pass :: typetochar => btoc
   end type

   contains

   subroutine btob ( a , b )
      class(base) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

   subroutine ctob ( a , b )
      character(3) :: b
      class(base)  :: a
      intent(out)  :: a
      intent(in)   :: b

      a%c = b

   end subroutine

   subroutine btoc ( a , b )
      character :: b(3)
      class(base)  :: a
      intent(out)  :: a
      intent(in)   :: b

      a = b(1) // b(2) // b(3)

   end subroutine

end module

program genericC459Assignment001d
end program
