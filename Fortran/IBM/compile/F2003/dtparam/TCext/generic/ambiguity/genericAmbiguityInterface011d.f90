! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityInterface011d.f
! opt variations: -qnol

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
!*  DESCRIPTION                : Within a scoping unit, if a generic name is the same as the name
!*                               of a generic intrinsic procedure, the generic intrinsic procedure
!*                               is not accessible if the procedures in the interface and the intrinsic
!*                               procedure are not all functions or are not all subroutines
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


module type

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   interface null
      subroutine externalnull(a)
         import base
         type(base(*,4)), intent(inout) :: a
      end subroutine
   end interface

end module

subroutine externalnull(a)
   use type, only: base
   type(base(*,4)), intent(inout) :: a

   a%i = 0

end subroutine

program genericAmbiguityInterface011d
   use type

   integer, pointer :: i => null()
   integer, pointer :: j

   j => null()

end program
