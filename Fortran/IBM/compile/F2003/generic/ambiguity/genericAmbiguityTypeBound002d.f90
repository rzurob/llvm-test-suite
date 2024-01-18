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
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : one pass argument specified (for generic-name tb)
!*
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
      integer :: i, j
      contains
         procedure, pass :: printi
         procedure, pass :: pj => printj
         generic :: print => printi, pj
   end type

   contains

      subroutine printi(a)
         class(base), intent(in) :: a
         print *, a%i
      end subroutine

      subroutine printj(b)
         class(base), intent(in) :: b
         print *, b%j
      end subroutine

end module


module unaryop

   type base1
      integer :: i,j
      contains
         procedure, pass :: negi
         procedure, pass :: nj => negj
         generic :: operator(-) => negi, nj
   end type

   contains

      type(base1) function negi(a)
         class(base1), intent(in) :: a

         negi = base1(-1*a%i,a%j)
      end function

      type(base1) function negj(b)
         class(base1), intent(in) :: b
         negj = base1(b%i,-1*b%j)
      end function

end module

program genericAmbiguityTypeBound002d
end program
