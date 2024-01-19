! GB DTP extension using:
! ftcx_dtp /tstdev/OO_type/abstract/C503/interface002.f -qck -qk -ql
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: interface block
!*                                        non-poly abstract type return and dummy argument, interface of an external procedure
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
end type

end module

type(base(4)) function foo(dtv)
   use m
   type(base(4)) :: dtv
end function

program interface002
   use m, newbase => base

   interface
      type(newbase(4)) function foo(dtv)
         import newbase
         type(newbase(4)) :: dtv
      end function
   end interface

end program


