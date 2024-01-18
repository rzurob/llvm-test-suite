!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTypBndGenName.f
!*
!*  DATE                       : Jul. 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- Generic name
!*  (323671/325783)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE Mod

  TYPE :: DT
    CHARACTER :: ID
    CONTAINS
    GENERIC    :: GName => ModFun
    PROCEDURE  :: ModFun
  END TYPE

  INTERFACE  GName
  ELEMENTAL  FUNCTION ExtFun(Arg1, Arg2)
      IMPORT DT
      TYPE(DT), INTENT(IN) :: Arg1
      TYPE(DT), INTENT(IN) :: Arg2
      CHARACTER            :: ExtFun
    END FUNCTION
    PROCEDURE ExtFun
  END INTERFACE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT), INTENT(IN) :: Arg
  CHARACTER             :: ModFun
    ModFun = Arg%ID
  END FUNCTION

  END MODULE

  ELEMENTAL FUNCTION ExtFun(Arg1, Arg2)
  USE Mod, ONLY: DT
  TYPE(DT), INTENT(IN) :: Arg1
  TYPE(DT), INTENT(IN) :: Arg2
  CHARACTER            :: ExtFun
    ExtFun =  Arg2%ID
  END FUNCTION


  PROGRAM dataPtrTypBndGenName
  USE Mod
  IMPLICIT NONE

  INTEGER            :: I, J, K
  INTEGER, PARAMETER :: N=10, M=100

  TYPE(DT), TARGET  :: Tar1(M)=(/(DT(ID=CHAR(I)), I=M,1,-1)/)
  TYPE(DT), TARGET  :: Tar2(N,N)=RESHAPE((/(DT(ID=CHAR(I)), I=1,M)/),(/N,N/))
  TYPE(DT), POINTER :: Ptr(:, :)
  TYPE(DT)          :: T1(M)=DT(ID=CHAR(0))
  TYPE(DT)          :: T2(N,N)=DT(ID=CHAR(0))


  Ptr(1:,1:) => Tar2
  IF (.NOT. ASSOCIATED(Ptr, Tar2))                                            STOP 11
  IF (ANY( LBOUND(Ptr) .NE. (/1, 1 /)))                                       STOP 12
  IF (ANY( UBOUND(Ptr) .NE. (/N,N/)))                                         STOP 13
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=1,M)/),(/N,N/))))           STOP 14
  IF (ANY( Ptr%GName()  .NE. RESHAPE((/(CHAR(I), I=1,M)/),(/N,N/))))          STOP 15

  Ptr(0:9,0:9) => Tar1
  IF (.NOT. ASSOCIATED(Ptr))                                                  STOP 31
  IF (ANY( LBOUND(Ptr) .NE. (/0, 0 /)))                                       STOP 32
  IF (ANY( UBOUND(Ptr) .NE. (/9,9/)))                                         STOP 33
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=M,1,-1)/),(/N,N/))))        STOP 34
  IF (ANY( Ptr%GName(   )  .NE. RESHAPE((/(CHAR(I), I=M,1,-1)/),(/N,N/))))    STOP 35
  IF (ANY( GName(Ptr,Ptr)  .NE. RESHAPE((/(CHAR(I), I=M,1,-1)/),(/N,N/))))    STOP 36

  END

