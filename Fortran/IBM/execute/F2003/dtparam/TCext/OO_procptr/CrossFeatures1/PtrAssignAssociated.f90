! GB DTP extension using:
! ftcx_dtp -qnodeferredlp /tstdev/OO_procptr/CrossFeatures1/PtrAssignAssociated.f
! opt variations: -qck -qdeferredlp

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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,N1)    ! (4,3)
    INTEGER, KIND            :: K1
    INTEGER, LEN             :: N1
    INTEGER(K1), ALLOCATABLE :: IArr(:)
    CHARACTER(N1)            :: Str
  END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(DT(4,*))            :: Arg(:)
    TYPE(DT(4,3)), POINTER   :: ModFun(:)
      !ALLOCATE(ModFun(SIZE(Arg)), SOURCE=Arg) ! not 10.1
      ALLOCATE(ModFun(SIZE(Arg)))
      ModFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM PtrAssignAssociated
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION IFun(Arg)
    IMPORT
    TYPE(DT(4,*))          :: Arg(:)
    TYPE(DT(4,3)), POINTER :: IFun(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun), POINTER :: ProcPtr
  PROCEDURE(IFun), POINTER :: ProcPtr1
  INTEGER :: i

  !ProcPtr => NULL()
  ProcPtr => NULL(ProcPtr)

  IF ( ASSOCIATED(ProcPtr) )                ERROR STOP 11
  IF ( ASSOCIATED(ProcPtr, ModFun) )        ERROR STOP 12
  IF ( ASSOCIATED(ProcPtr, ProcPtr) )       ERROR STOP 13
! IF ( ASSOCIATED(ProcPtr, NULL()) )        ERROR STOP 14
  IF ( ASSOCIATED(ProcPtr, NULL(ProcPtr)) ) ERROR STOP 15

  ProcPtr => ModFun
  IF ( .NOT. ASSOCIATED(ProcPtr) )          ERROR STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr, ModFun) )  ERROR STOP 22

  ASSOCIATE ( As => ProcPtr( (/DT(4,3)((/(i, i=1,256)/), "IBM")/)) )
    IF ( ANY( As(1)%IARR .NE. (/(i, i=1,256)/)))    ERROR STOP 31
    IF ( As(1)%Str       .NE. "IBM" )               ERROR STOP 32
  END ASSOCIATE

  NULLIFY(ProcPtr)

  ProcPtr1 => NULL()
  ProcPtr => ProcPtr1

  IF ( ASSOCIATED(ProcPtr) ) ERROR STOP 41
  IF ( ASSOCIATED(ProcPtr, ModFun) )        ERROR STOP 42
  IF ( ASSOCIATED(ProcPtr, ProcPtr) )       ERROR STOP 43
! IF ( ASSOCIATED(ProcPtr, NULL()) )        ERROR STOP 44
  IF ( ASSOCIATED(ProcPtr, NULL(ProcPtr)) ) ERROR STOP 45

  ProcPtr1  => ModFun
  ProcPtr => ProcPtr1

  IF ( .NOT. ASSOCIATED(ProcPtr) )         ERROR STOP 51
  IF ( .NOT. ASSOCIATED(ProcPtr, ModFun) ) ERROR STOP 52

  ASSOCIATE ( As => ProcPtr((/ DT(4,3)((/(i, i=1,25600)/), "IBM")/)) )
    IF ( ANY( As(1)%IARR .NE. (/(i, i=1,25600)/) ))   ERROR STOP 61
    IF ( As(1)%Str       .NE. "IBM" )                 ERROR STOP 62
  END ASSOCIATE


  END

