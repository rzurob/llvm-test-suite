! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/TypeDecl1.f
! opt variations: -qck -qnok

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: TypeDecl1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  TypeDecl1.f
!*
!*  DATE                       : Jun. 07, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Function-names declared shall be the name of an external function
!*  or function dummy procedure
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: Base(K1,N1)    ! (4,3)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: C
  END TYPE

  TYPE :: Base1(K2,N2)    ! (4,3)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    CHARACTER(N2) :: C
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  END MODULE

  FUNCTION ExtFun()
  USE M
  TYPE(Base(4,3)) :: ExtFun
    ExtFun = Base(4,3)("123")
  END FUNCTION

  FUNCTION ExtFun1()
  USE M
  TYPE(Base1(4,3))  :: ExtFun1
  PROCEDURE()  :: ExtSub
    ExtFun1 = Base1(4,3)("123", ExtSub)
  END FUNCTION

  SUBROUTINE ExtSub()
  END SUBROUTINE

  PROGRAM TypeDecl1
  USE M
  IMPLICIT TYPE(Base(4,3))(P)

  PROCEDURE(TYPE(Base(4,3))), POINTER :: ProcPtr => NULL()

  PROCEDURE() :: ExtSub

  PROCEDURE() :: ExtFun
  TYPE(Base(4,3))  :: ExtFun
  TYPE(Base(4,3))  :: V

  TYPE(Base1(4,3)) :: ExtFun1
  TYPE(Base1(4,3)) :: U
  PROCEDURE() :: ExtFun1

  V = ExtFun()
  IF ( V%C .NE. "123" ) STOP 11

  U = ExtFun1()
  IF ( U%C .NE. "123" ) STOP 12
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ExtSub) ) STOP 13

  END


