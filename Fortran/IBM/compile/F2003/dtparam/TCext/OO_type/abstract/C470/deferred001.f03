! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C470 (R453) DEFERRED shall appear if and only if interface-name appears.
!*                               Specified interface-name but not deferred binding
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
   type, abstract:: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure(inf), nopass :: print
   end type

   interface
      subroutine inf
      end subroutine
   end interface

contains

   subroutine print()
      print *,"hello"
   end subroutine

end module

program deferred001

end program

