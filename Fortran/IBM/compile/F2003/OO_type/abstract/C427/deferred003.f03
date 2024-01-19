! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: if the type definition contains or inherits
!*                                        a deferred binding, ABSTRACT shall appear. (C427)
!*                                        iii) Type definition inherits a deferred binding which was overridden by a deferred binding, ABSTRACT not defined
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

   type, abstract :: b1
      integer :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b1) :: b2
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b2) :: b3
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

end module

program deferred003

end program

subroutine printif()
   print *,'hello'
end subroutine