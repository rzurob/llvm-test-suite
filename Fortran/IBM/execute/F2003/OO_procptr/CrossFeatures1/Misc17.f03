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


  TYPE :: DT
    SEQUENCE
    INTEGER :: ID=One
    PROCEDURE(Modfun), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  INTEGER :: Arg(:)
  INTEGER :: ModFun(SIZE(Arg))
    ModFun = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  TYPE(DT) :: Arg(:)
  TYPE(DT) :: ModFun1(SIZE(Arg))
    ModFun1 = Arg
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)

  TYPE :: DT
    SEQUENCE
    INTEGER :: ID=1
    PROCEDURE(IntFun), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  TYPE(DT) :: Arg(:)
  TYPE(DT) :: ExtFun(SIZE(Arg))

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

  PROCEDURE (ModFun1)          :: ExtFun
  PROCEDURE(ModFun1), POINTER  :: ProcPtr
  TYPE(DT)                     :: ProcPtr
  TYPE(DT)                     :: U(10000)

  IF ( ANY(U%ID .NE. 1 )) ERROR STOP 11

  U = DT( Thr, ModFun )
  DO I  = 1, 10000
    IF ( U(I)%Id .NE. 3 )                         ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Modfun) ) ERROR STOP 23
    IF ( ANY(U(I)%ProcPtr((/One, Two, Thr/)) .NE. (/One, Two, Thr/) )) ERROR STOP 24
  END DO


  ProcPtr => Extfun
  U = ProcPtr( (/(DT(-I, ModFun), I=1, 10000) /) )

  DO I  = 1, 10000
    IF ( U(I)%Id .NE. -I )                         ERROR STOP 32
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Modfun) )  ERROR STOP 33
    IF ( ANY(U(I)%ProcPtr((/One, Two, Thr/)) .NE. (/One, Two, Thr/) )) ERROR STOP 34
  END DO

  END

