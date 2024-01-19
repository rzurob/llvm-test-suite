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
!*                                        ii) Type definition inherits a deferred binding, ABSTRACT not defined
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

   type, abstract :: b1(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b1) :: b2(k2,n)
      integer, kind :: k2
      integer, len :: n
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

end module

program deferred002

end program

subroutine printif()
   print *,'hello'
end subroutine
