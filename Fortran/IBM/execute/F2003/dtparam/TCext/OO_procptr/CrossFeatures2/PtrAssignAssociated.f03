! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/OO_procptr/CrossFeatures2/PtrAssignAssociated.f
! opt variations: -qnock -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  The procedure pointer's status
!*  (315166/315827)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,N1)    ! (1,3)
    INTEGER, KIND                          :: K1
    INTEGER, LEN                           :: N1
    CHARACTER(kind=K1,len=N1), ALLOCATABLE :: C
  END TYPE

  CONTAINS
    FUNCTION ModFun(Arg)
    CLASS(DT(1,*))          :: Arg(:,:)
    CLASS(DT(1,:)), POINTER :: ModFun(:,:)
      ALLOCATE(ModFun(SIZE(Arg, 1), SIZE(Arg, 2)), SOURCE=Arg)
    END FUNCTION

  END MODULE


  PROGRAM PtrAssignAssociated
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION IFun(Arg)
    IMPORT
    CLASS(DT(1,*))          :: Arg(:,:)
    CLASS(DT(1,:)), POINTER :: IFun(:,:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun), POINTER :: ProcPtr
  PROCEDURE(IFun), POINTER :: ProcPtr1
  INTEGER :: i, j

  ProcPtr => NULL()

  IF ( ASSOCIATED(ProcPtr) )                ERROR STOP 11
  IF ( ASSOCIATED(ProcPtr, ModFun) )        ERROR STOP 12
  IF ( ASSOCIATED(ProcPtr, ProcPtr) )       ERROR STOP 13
  IF ( ASSOCIATED(ProcPtr, NULL()) )        ERROR STOP 14
  IF ( ASSOCIATED(ProcPtr, NULL(ProcPtr)) ) ERROR STOP 15

  ProcPtr => ModFun
  IF ( .NOT. ASSOCIATED(ProcPtr) )          ERROR STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr, ModFun) )  ERROR STOP 22

  SELECT TYPE ( As => ProcPtr(RESHAPE((/( DT(1,3)("IBM"), i=1,256)/), (/16,16 /)) ))
  TYPE IS (DT(1,*))
    IF ( ANY( SHAPE(As)   .NE. (/16,16/) )) ERROR STOP 31
    DO i=1, 16
    DO j=1, 16
      IF ( As(i, j)%C     .NE. "IBM"    )   ERROR STOP 32
    END DO
    END DO
  CLASS DEFAULT
    STOP 33
  END SELECT

  NULLIFY(ProcPtr)

  ProcPtr1  => NULL()
  ProcPtr => ProcPtr1

  IF ( ASSOCIATED(ProcPtr) ) ERROR STOP 41
  IF ( ASSOCIATED(ProcPtr, ModFun) )        ERROR STOP 42
  IF ( ASSOCIATED(ProcPtr, ProcPtr) )       ERROR STOP 43
  IF ( ASSOCIATED(ProcPtr, NULL()) )        ERROR STOP 44
  IF ( ASSOCIATED(ProcPtr, NULL(ProcPtr)) ) ERROR STOP 45

  ProcPtr1  => ModFun
  ProcPtr => ProcPtr1

  IF ( .NOT. ASSOCIATED(ProcPtr) )         ERROR STOP 51
  IF ( .NOT. ASSOCIATED(ProcPtr, ModFun) ) ERROR STOP 52

  SELECT TYPE ( As => ProcPtr(RESHAPE((/( DT(1,3)("123"), i=1,256)/), (/16,16 /)) ))
  TYPE IS (DT(1,*))
    IF ( ANY( SHAPE(As) .NE. (/16,16/) ))  ERROR STOP 53
    DO i=1, 16
    DO j=1, 16
      IF ( As(i, j)%C     .NE. "123"    )  ERROR STOP 54
    END DO
    END DO
  CLASS DEFAULT
    STOP 55
  END SELECT

  END
