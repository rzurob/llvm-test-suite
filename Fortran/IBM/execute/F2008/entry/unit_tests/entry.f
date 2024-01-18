!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qlanglvl=2008pure -qlanglvl=2008std
! %GROUP: entry.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      subroutine sub ()
        ENTRY area
      end

!*  ===================================================================
!*
!*  DATE                       : Oct 26, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!     A message is emitted for obsolescent ENTRY statement only when using -qlanglvl=2008pure
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :  -qlanglvl=2008pure
!*                             :  -qlanglvl=2008std
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
