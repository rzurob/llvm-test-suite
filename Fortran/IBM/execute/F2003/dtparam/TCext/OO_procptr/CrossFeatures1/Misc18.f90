! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Misc18.f
! opt variations: -qnol

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

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    INTEGER(K1)   :: ID
    PROCEDURE(ModFun), POINTER, NOPASS :: ProcPtr
  END TYPE

  PROCEDURE(ModFun1)           :: ExtFun
  PROCEDURE(ModFun1), POINTER  :: ProcPtr
  TYPE(DT(20,4))                     :: V(10000)

  CONTAINS

  FUNCTION ModFun(Arg)
  INTEGER :: Arg(:)
  INTEGER :: ModFun(SIZE(Arg))
    ModFun = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  TYPE(DT(*,4)) :: Arg(:)
  TYPE(DT(20,4)) :: ModFun1(SIZE(Arg))
    ModFun1 = Arg
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)

  TYPE :: DT(N2,K2)    ! (20,4)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    SEQUENCE
    INTEGER(K2)   :: ID
    PROCEDURE(IntFun), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  TYPE(DT(*,4)) :: Arg(:)
  TYPE(DT(20,4)) :: ExtFun(SIZE(Arg))

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
  TYPE(T(*,4)) :: Arg(:)
  TYPE(T(20,4)) :: ModFun1(SIZE(Arg))
  END FUNCTION
  end interface

  PROCEDURE(IntF) :: ExtFun
  INTEGER         :: I
  TYPE (T(20,4))        :: V1(3), V2(3)

  U = T(20,4)(-1, MFun)

  DO I  = 1, 10000
    IF ( U(I)%Id .NE. -1 )                             ERROR STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, MFun) )        ERROR STOP 23
    IF ( ANY(U(I)%ProcPtr((/1,2,3/)) .NE. (/1,2,3/) )) ERROR STOP 24
  END DO

  Ptr => EFun
  U = Ptr( (/(T(20,4)(-I, MFun), I=1, 10000) /) )

  DO I  = 1, 10000
    IF ( U(I)%Id .NE. -I )                       ERROR STOP 32
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, MFun) )  ERROR STOP 33
    IF ( ANY(U(I)%ProcPtr((/1,2,3/)) .NE. (/1,2,3/) )) ERROR STOP 34
  END DO

  V1 = ExtFun((/T(20,4)(-1, MFun),T(20,4)(-1, MFun), T(20,4)(-1, MFun)/))
  V2 = EFun((/T(20,4)(-1, MFun),T(20,4)(-1, MFun), T(20,4)(-1, MFun)/))

  DO I = 1,3
    IF ( V1(I)%ID .NE. V2(I)%ID )  ERROR STOP 44
    IF ( .NOT. ASSOCIATED(V1(I)%ProcPtr, V2(I)%ProcPtr)  )  ERROR STOP 45
  END DO

  END


