! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Misc9.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc9
!*
!*  DATE                       : Nov. 12, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTIOa
!*
!*  Unknown entity as type guard of select type caused ICE.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  TYPE t
  END TYPE

  CLASS(t), ALLOCATABLE :: Y

  ALLOCATE(Y)
  SELECT TYPE ( Y)
    type is (W) ! Unknown entity
      print*, "wrong!"
  END SELECT

  END

