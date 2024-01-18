! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Misc/Misc27.f
! opt variations: -qnok -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc27.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc27
!*
!*  DATE                       : Feb 03, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Specification expression
!*  (299523)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc27
  TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  INTEGER :: V = 1

  TYPE :: Test(N2,K2)    ! (20,4)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    LOGICAL(K2)   :: W(SIZE((/V/)))
  END TYPE

  END





