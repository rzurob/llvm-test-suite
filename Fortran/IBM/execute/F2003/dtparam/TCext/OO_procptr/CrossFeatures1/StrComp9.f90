! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/StrComp9.f
! opt variations: -qck -qnok

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 18, 2005
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
!*  Procedure pointer components - sequence
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      SEQUENCE
      CHARACTER(N1) :: C
      PROCEDURE(LOGICAL(1)), NOPASS, POINTER :: ProcPtr
    END TYPE

    INTERFACE
      FUNCTION ExtFun(C, ProcPtr)
        IMPORT
        TYPE(Base(4,3))           :: ExtFun
        CHARACTER(3)         :: C
        PROCEDURE(LOGICAL(1)), POINTER :: ProcPtr
      END FUNCTION
    END INTERFACE

  CONTAINS

  FUNCTION ModFun()
  LOGICAL(1) :: ModFun
    ModFun = .TRUE.
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(C, ProcPtr)

  TYPE :: Base(K2,N2)    ! (4,3)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    SEQUENCE
    CHARACTER(N2) :: C
    PROCEDURE(LOGICAL(1)), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(Base(4,3))           :: ExtFun
  CHARACTER(3)         :: C
  PROCEDURE(LOGICAL(1)), POINTER :: ProcPtr
    ExtFun%ProcPtr => ProcPtr
    ExtFun%C = C

  END FUNCTION


  PROGRAM StrComp8
  USE M
  IMPLICIT TYPE(Base(4,3))(P)

  TYPE(Base(4,3))  :: U
  LOGICAL     :: L=.FALSE.

  U = Base(4,3)("123", ModFun )
  IF ( U%C .NE. "123" )                       STOP 21
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun ))  STOP 22
  IF ( .NOT. U%ProcPtr())                     STOP 23

  U =  Base(4,3)("", NULL() )
  IF ( U%C .NE. "" )        STOP 31
  IF ( ASSOCIATED(U%ProcPtr )) STOP 32


  END

