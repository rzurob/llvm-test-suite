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
!*  Rename entities in module
!*
!* (305850) (306564)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT INTEGER(P)

  TYPE :: DT
    SEQUENCE
    INTEGER                            :: ID
    PROCEDURE(ModFun), POINTER, NOPASS :: ProcPtr
  END TYPE

  PROCEDURE(ModFun1)           :: ExtFun
  PROCEDURE(ModFun1), POINTER  :: ProcPtr
  TYPE(DT)                     :: V(10000)

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
    INTEGER                            :: ID
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


  PROGRAM Misc18
  USE M, EFun => ExtFun,     &
       & MFun => ModFun,     &
       & IntF => ModFun1,    &
       & Ptr  => ProcPtr,    &
       & T    => DT,         &
       & U    => V

  IMPLICIT NONE
  interface
  FUNCTION ModFun1(Arg)
  import
  TYPE(T) :: Arg(:)
  TYPE(T) :: ModFun1(SIZE(Arg))
  END FUNCTION
  end interface

  PROCEDURE(IntF) :: ExtFun
  INTEGER         :: I
  TYPE (T)        :: V1(3), V2(3)

  U = T(-1, MFun)

  DO I  = 1, 10000
    IF ( U(I)%Id .NE. -1 )                             STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, MFun) )        STOP 23
    IF ( ANY(U(I)%ProcPtr((/1,2,3/)) .NE. (/1,2,3/) )) STOP 24
  END DO

  Ptr => EFun
  U = Ptr( (/(T(-I, MFun), I=1, 10000) /) )

  DO I  = 1, 10000
    IF ( U(I)%Id .NE. -I )                       STOP 32
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, MFun) )  STOP 33
    IF ( ANY(U(I)%ProcPtr((/1,2,3/)) .NE. (/1,2,3/) )) STOP 34
  END DO

  V1 = ExtFun((/T(-1, MFun),T(-1, MFun), T(-1, MFun)/))
  V2 = EFun((/T(-1, MFun),T(-1, MFun), T(-1, MFun)/))

  DO I = 1,3
    IF ( V1(I)%ID .NE. V2(I)%ID )  STOP 44
    IF ( .NOT. ASSOCIATED(V1(I)%ProcPtr, V2(I)%ProcPtr)  )  STOP 45
  END DO

  END


