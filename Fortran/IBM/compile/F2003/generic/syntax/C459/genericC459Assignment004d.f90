!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
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
!*  DESCRIPTION                : C459: using private-binding-stmt
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
      integer :: i = -999
      contains
         private
         generic :: assignment(=) => btob
         procedure, pass ( a ) :: btob
         generic, public :: assignment(=) => btoi
         procedure, pass( a ) :: btoi
   end type

   contains

   subroutine btob ( a, b )
      class(base) :: a, b
      intent( in ) :: b
      intent( out )  :: a

      a%i = b%i + 1

   end subroutine

   subroutine btoi ( a, b )
      class(base) :: a
      integer :: b
      intent( in ) :: b
      intent( out )  :: a

      a%i = b + 3

   end subroutine

end module

program genericC459Assignment004d
end program
