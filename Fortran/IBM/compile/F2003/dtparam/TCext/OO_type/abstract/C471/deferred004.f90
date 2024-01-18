! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C471/deferred004.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp deferred004.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Overridding
!*                                        deferred binding overridden by final procedure
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
   procedure(itf), pass, deferred :: final
end type

type, extends(base) :: child(k2,n1)    ! (4,4,20)
    integer, kind :: k2
    integer, len  :: n1
contains
   final :: final
end type

interface
   subroutine itf(a)
      import base
      class(base(4)), intent(inout) :: a
   end subroutine
end interface

contains
   subroutine final(a)
      type(child(4,4,*)), intent(inout) :: a
      a%id=0
      print *,'finalizechild'
   end subroutine
end module

program deferred004

end program
