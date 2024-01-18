!************************************************************************
      program IEEESelRKindDiag2
        USE, INTRINSIC :: IEEE_ARITHMETIC

        print *, ieee_selected_real_kind(6, 6, 6)
      end

!*  ===================================================================
!*
!*  DATE                       : Nov 06, 2010
!*                             : IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!     A message is emitted for langlvl error with 3 arguments specified
!     of ieee_selected_real_kind intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :  -qlanglvl=2003std
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
