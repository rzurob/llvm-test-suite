! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C469 non_overridable and deferred shall not both appear in the same binding-attr-list
!*                                        declare non_overridable and deferred at the same time
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure(printif), deferred, non_overridable, nopass :: print
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

end module

program nonOverride001

end program
