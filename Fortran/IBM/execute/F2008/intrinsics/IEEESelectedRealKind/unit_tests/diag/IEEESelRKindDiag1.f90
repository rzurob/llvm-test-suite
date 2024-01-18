!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qlanglvl=2003pure
! %GROUP: IEEESelRKindDiag1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      program IEEESelRKindDiag1
        USE, INTRINSIC :: IEEE_ARITHMETIC

        print *, ieee_selected_real_kind(3, 3, 3)
      end

!*  ===================================================================
!*
!*  DATE                       : Nov 06, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!     A message is emitted for langlvl error with 3 arguments specified
!     of ieee_selected_real_kind intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :  -qlanglvl=2003pure
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
