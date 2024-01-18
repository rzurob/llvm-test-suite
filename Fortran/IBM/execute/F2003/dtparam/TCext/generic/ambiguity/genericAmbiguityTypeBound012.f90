! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound012.f
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
!*  DESCRIPTION                : two argument with pass-arg to be first/second arg specified (for generic-name, operator, and assignment tb)
!*                                  - one different independent types, ambiguous type bound, but type do not collide
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

   type b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass(a) :: printa
         generic :: print => printa
   end type

   type b2(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, pass(b) :: printb
         generic :: print => printb
   end type

   contains

      subroutine printa(a, b)
         class(b1(4)), intent(in) :: a
         type(b2(4)),  intent(in) :: b

         print *, 'printa: ', a%i, b%i

      end subroutine

      subroutine printb(a, b)
         type(b1(4)), intent(in)  :: a
         class(b2(4)), intent(in) :: b

         print *, 'printb: ', a%i, b%i

      end subroutine

end module

program genericAmbiguityTypeBound012
   use m

   type(b1(4)) :: b11 = b1(4)(10)
   type(b2(4)) :: b12 = b2(4)(20)

   call b11%print(b12)
   call b12%print(b11)

end program
