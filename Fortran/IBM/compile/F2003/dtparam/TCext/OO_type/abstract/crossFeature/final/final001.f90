! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/final/final001.f
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
! %POSTCMD: dcomp final001.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Final Subroutines
!*                               Define Final Subroutine for abstract type
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
   type , abstract:: base1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
   contains
      final :: finalbase1
   end type

   type, abstract :: base2(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: k
   contains
      final :: finalbase2
   end type

contains

   subroutine finalbase1(a)
      type(base1(4)), intent(in) :: a
      print *,"finalizebase"
   end subroutine

   subroutine finalbase2(a)
      class(base2(4)), intent(in) :: a
      print *,"finalizebase"
   end subroutine

end module

program final001

end program


