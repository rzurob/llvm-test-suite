! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C471 An overriding binding shall have the DEFERRED attribute only if the binding
!*                                        it overrides is deferred.
!*                                        Overriding a non-deferred binding with deferred binding
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type:: base
      integer :: id
   contains
      procedure, nopass :: print1 => printbase
   end type

   type, extends(base), abstract :: child
   contains
      procedure(printif), deferred,  nopass :: print1
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

contains
   subroutine printbase()
      print *,"hello"
   end subroutine
end module

program deferred001

end program
