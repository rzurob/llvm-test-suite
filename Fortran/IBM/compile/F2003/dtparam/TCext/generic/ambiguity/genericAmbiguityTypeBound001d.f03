! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound001d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : no arguments specified (for generic-name tb and generic interface)
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i, j
      contains
         procedure, nopass :: printa
         procedure, nopass :: b => printb
         generic :: print => printa, b
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

   interface base
      module procedure printa1
      subroutine printb1()
      end subroutine
   end interface

   contains

      subroutine printa1()
         print *, 'a1'
      end subroutine

end module

subroutine printb1()
   print *, 'b1'
end subroutine

program genericAmbiguityTypeBound001d
end program
