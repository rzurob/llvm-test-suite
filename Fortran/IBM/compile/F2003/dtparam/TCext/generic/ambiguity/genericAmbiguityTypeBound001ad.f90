! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound001ad.f
! opt variations: -ql -qreuse=none

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
!*  DESCRIPTION                : no arguments specified, and extended type
!*                               defining a ambiguous tb (for generic-name tb and generic interface)
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
      integer(k1)   :: i
      contains
         procedure, nopass :: printa
         generic :: print => printa
   end type

   contains

      subroutine printa()
         print *, 'a'
      end subroutine

      subroutine printb()
         print *, 'b'
      end subroutine

end module

module n
   use m, only: newbase=>base, printb
   
   type,extends(newbase) :: child    ! (4)
      integer(k1) :: j
      contains
         procedure, nopass :: printb
         generic :: print => printb
   end type

end module


program genericAmbiguityTypeBound001ad
end program
