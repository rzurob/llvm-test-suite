! GB DTP extension using:
! ftcx_dtp /tstdev/OO_type/abstract/C503/interface001.f -qck -qk -ql
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: interface block
!*                                        non-poly abstract type return, interface of deferred binding
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

type, abstract :: base(k1)    ! (4)
   integer, kind :: k1
   integer(k1)      id
contains
   procedure(itf), nopass, deferred :: getid
end type

interface
   type(base(4)) function itf()
      import base
   end function
end interface

end module

program interface001

end program
