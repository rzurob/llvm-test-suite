!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : GENERICS
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C461: Generic type bound with operator and do not
!*                                     specify pass object dummy argument
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
      integer :: i = -1
   contains
      procedure, nopass, private :: typetotype
      generic :: assignment(=) => typetotype, inttotype
      procedure, nopass :: inttotype
   end type

   contains

   subroutine typetotype ( a, b )
      class(base), intent(out) :: a   
      class(base), intent(in)  :: b
      
      a%i = b%i
   end subroutine
   
   subroutine inttotype ( a, b )
      class(base), intent(out) :: a   
      integer(4), intent(in)  :: b
      
      a%i = b
   end subroutine

end module

program genericC461Assignment001d
end program
