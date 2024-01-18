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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: optional dummy arguments and ambiguity
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
      contains
         procedure, pass :: optional1
         procedure, pass :: optional2
         generic :: print => optional1, optional2
   end type

   contains

      subroutine optional1 ( a ,b )
         class(base), intent(in) :: a, b

      end subroutine

      subroutine optional2 ( a, b )
         class(base), intent(in) :: a
         class(base), intent(in), optional :: b

      end subroutine

end module

program genericGenericNameOptional003d
end program
