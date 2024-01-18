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

   type :: base
      character(3) :: c
      contains
         generic, private :: assignment(=) => typetotype
         procedure, pass, private :: typetotype => btob
   end type

   type, extends(base) :: child
      integer :: i
      contains
         procedure, pass :: typetotype => ctoc
   end type

   contains

   subroutine btob ( a , b )
      class(base) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

   subroutine ctoc ( a , b )
      class(child) :: a
      class(base) :: b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

end module

module n
   use m, only: child

   type, extends(child) :: gen3
      integer j
      contains
         generic :: assignment(=) => gtog
         procedure, pass :: gtog => gtog
   end type

   contains

   subroutine gtog ( a , b )
      class(gen3) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c
      a%i = b%i
      a%j = b%j

   end subroutine

end module

program genericC459Assignment002
end program
