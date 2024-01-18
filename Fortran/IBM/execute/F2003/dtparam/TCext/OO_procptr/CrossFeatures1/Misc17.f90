! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Misc17.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2005
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
!*  Combination declaration of proc ptr
!*
!*  (MemFault)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT INTEGER(P)

  ENUM, BIND(C)
    ENUMERATOR :: One=1
    ENUMERATOR :: Two
    ENUMERATOR :: Thr
  END ENUM


  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    SEQUENCE
    INTEGER(K1)   :: ID=One
    PROCEDURE(Modfun), POINTER, NOPASS :: ProcPtr => NULL()
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

  FUNCTION ExtFun(Arg)

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    SEQUENCE
    INTEGER(K1)   :: ID=1
    PROCEDURE(IntFun), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  TYPE(DT(4)) :: Arg(:)
  TYPE(DT(4)) :: ExtFun(SIZE(Arg))

  ExtFun = Arg

  CONTAINS

  FUNCTION IntFun(Arg)
  INTEGER :: Arg(:)
  INTEGER :: IntFun(SIZE(Arg))
    IntFun = Arg
  END FUNCTION

  END FUNCTION

  PROGRAM Misc17
  USE M
  IMPLICIT INTEGER(P)

  PROCEDURE(ModFun1)          :: ExtFun
  PROCEDURE(ModFun1), POINTER :: ProcPtr
  TYPE(DT(4))                 :: U(10000)

  IF ( ANY(U%ID .NE. 1 )) STOP 11

  U = DT(4)( Thr, ModFun )
  DO I  = 1, 10000
    IF ( U(I)%Id .NE. 3 )                         STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Modfun) ) STOP 23
    IF ( ANY(U(I)%ProcPtr((/One, Two, Thr/)) .NE. (/One, Two, Thr/) )) STOP 24
  END DO


  ProcPtr => Extfun
  U = ProcPtr( (/(DT(4)(-I, ModFun), I=1, 10000) /) )

  DO I  = 1, 10000
    IF ( U(I)%Id .NE. -I )                         STOP 32
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Modfun) )  STOP 33
    IF ( ANY(U(I)%ProcPtr((/One, Two, Thr/)) .NE. (/One, Two, Thr/) )) STOP 34
  END DO

  END


