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
!*  SECONDARY FUNCTIONS TESTED : with Assignment( = )
!*
!*  DRIVER STANZA              : xlf95
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
         generic, private :: assignment(=) => ttob
         procedure :: ttob
         
   end type
   
   type, extends(base) :: child
      contains
         generic :: assignment(=) => ttoc
         procedure :: ttoc
   end type

   contains

   subroutine ttob ( a , b )
      class(base) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

   subroutine ttoc ( a , b )
      character(3) :: b
      class(child)  :: a
      intent(out)  :: a
      intent(in)   :: b

      a%c = b

   end subroutine

end module

program genericC459Assignment005d
end program
