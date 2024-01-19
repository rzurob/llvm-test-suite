! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C469 non_overridable and deferred shall not both appear in the same binding-attr-list
!*                                        override non_overridable binding with deferred binding
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
      procedure, non_overridable, nopass :: print
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

   type, abstract, extends(base) :: child(k2)
      integer, kind :: k2
   contains
      procedure(printif), deferred, nopass :: print
   end type
contains
   subroutine print()
      print *,"base"
   end subroutine
end module

program nonOverride002

end program
