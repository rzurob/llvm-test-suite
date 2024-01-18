!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C460: specific-binding does not exist
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
         generic :: assignment(=) => myassgn
         procedure, pass :: myassgn1 => myassgn
   end type

   contains

      subroutine myassgn(a, b)
         class(base), intent(out) :: a
         class(base), intent(in)  :: b

         a%i = b%i

      end subroutine myassgn

end module

program genericC460Assignment001d
end program
