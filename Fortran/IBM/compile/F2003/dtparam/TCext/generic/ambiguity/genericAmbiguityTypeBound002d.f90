! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound002d.f
! opt variations: -ql

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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i, j
      contains
         procedure, pass :: printi
         procedure, pass :: pj => printj
         generic :: print => printi, pj
   end type

   contains

      subroutine printi(a)
         class(base(4)), intent(in) :: a
         print *, a%i
      end subroutine

      subroutine printj(b)
         class(base(4)), intent(in) :: b
         print *, b%j
      end subroutine

end module


module unaryop

   type base1(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i,j
      contains
         procedure, pass :: negi
         procedure, pass :: nj => negj
         generic :: operator(-) => negi, nj
   end type

   contains

      type(base1(4)) function negi(a)
         class(base1(4)), intent(in) :: a

         negi = base1(4)(-1*a%i,a%j)
      end function

      type(base1(4)) function negj(b)
         class(base1(4)), intent(in) :: b
         negj = base1(4)(b%i,-1*b%j)
      end function

end module

program genericAmbiguityTypeBound002d
end program
