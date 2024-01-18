! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C503/interface004.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: interface004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: interface block
!*                                        poly abstract type return, interface of an external procedure
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
   integer(k1)   :: id
end type

type, extends(base) :: child(k2,n1)    ! (4,4,20)
    integer, kind :: k2
    integer, len  :: n1
end type

interface
   class(base(4)) function itf(a)
      import base
      class(base(4)), intent(in) :: a
      pointer :: itf
   end function
end interface

end module

program interface004
   use m

   class(base(4)), pointer :: b1
   type(child(4,4,20)) :: c1 = child(4,4,20)(5)
   procedure(itf) :: getbase

   b1 => getbase(c1)
   if(b1%id .ne. 5) error stop 1_4

end program

class(base(4)) function getbase(a)
   use m
   class(child(4,4,*)), intent(in) :: a
   pointer getbase
   allocate (getbase,source=a)
end function
