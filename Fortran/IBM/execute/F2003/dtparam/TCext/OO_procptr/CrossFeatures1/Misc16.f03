! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Misc16.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 09, 2005
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
!*  Enum
!*
!*  (Mem Fault)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT INTEGER(P)

  ENUM, BIND(C)
    ENUMERATOR :: Mon
    ENUMERATOR :: Tue
    ENUMERATOR :: Wed
    ENUMERATOR :: Thu
    ENUMERATOR :: Fri
    ENUMERATOR :: Sat
    ENUMERATOR :: Sun
  END ENUM

  ENUM, BIND(C)
    ENUMERATOR :: One
    ENUMERATOR :: Two
    ENUMERATOR :: Thr
  END ENUM


  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    SEQUENCE
    INTEGER(K1)   :: ID=One + Mon
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  INTEGER :: Arg(:)
  INTEGER :: ModFun(SIZE(Arg))
    ModFun = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  TYPE(DT(4)) :: Arg(:)
  TYPE(DT(4)) :: ModFun1(SIZE(Arg))
    ModFun1 = Arg
  END FUNCTION

  END MODULE

  PROGRAM Misc16
  USE M
  IMPLICIT INTEGER(P)

  PROCEDURE(ModFun1), POINTER :: ProcPtr
  TYPE(DT(4))                   :: U(10000)

  IF ( ANY(U%ID .NE. 0 )) ERROR STOP 11

  U = DT(4)( Thr, NULL() )

  U(1:6) = U((/Tue, Wed, Thu, Fri, Sat, Sun /) )
  IF ( ANY(U((/Tue, Wed, Thu, Fri, Sat, Sun /))%ID .NE. 2 )) ERROR STOP 12

  ProcPtr => ModFun1
  U = ProcPtr( (/(DT(4)(I, ModFun), I=1, 10000) /) )

  DO I  = 1, 10000
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Modfun) ) ERROR STOP 22
    IF ( U(I)%Id .NE. I )                         ERROR STOP 23
  END DO

  END

