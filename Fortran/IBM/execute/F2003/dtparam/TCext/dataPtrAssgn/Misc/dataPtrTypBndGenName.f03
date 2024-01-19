! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/dataPtrAssgn/Misc/dataPtrTypBndGenName.f
! opt variations: -qnock -qnodeferredlp

!*********************************************************************
!*  ===================================================================
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

  TYPE :: DT(K1,N1)    ! (1,1)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: ID
    CONTAINS
    GENERIC    :: GName => ModFun
    PROCEDURE  :: ModFun
  END TYPE

  INTERFACE  GName
  ELEMENTAL  FUNCTION ExtFun(Arg1, Arg2)
      IMPORT DT
      TYPE(DT(1,*)), INTENT(IN) :: Arg1
      TYPE(DT(1,*)), INTENT(IN) :: Arg2
      CHARACTER            :: ExtFun
    END FUNCTION
    PROCEDURE ExtFun
  END INTERFACE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT(1,*)), INTENT(IN) :: Arg
  CHARACTER             :: ModFun
    ModFun = Arg%ID
  END FUNCTION

  END MODULE

  ELEMENTAL FUNCTION ExtFun(Arg1, Arg2)
  USE Mod, ONLY: DT
  TYPE(DT(1,*)), INTENT(IN) :: Arg1
  TYPE(DT(1,*)), INTENT(IN) :: Arg2
  CHARACTER            :: ExtFun
    ExtFun =  Arg2%ID
  END FUNCTION


  PROGRAM dataPtrTypBndGenName
  USE Mod
  IMPLICIT NONE

  INTEGER            :: I, J, K
  INTEGER, PARAMETER :: N=10, M=100

  TYPE(DT(1,1)), TARGET  :: Tar1(M)=(/(DT(1,1)(ID=CHAR(I)), I=M,1,-1)/)
  TYPE(DT(1,1)), TARGET  :: Tar2(N,N)=RESHAPE((/(DT(1,1)(ID=CHAR(I)), I=1,M)/),(/N,N/))
  TYPE(DT(1,:)), POINTER :: Ptr(:, :)
  TYPE(DT(1,1))          :: T1(M)=DT(1,1)(ID=CHAR(0))
  TYPE(DT(1,1))          :: T2(N,N)=DT(1,1)(ID=CHAR(0))


  Ptr(1:,1:) => Tar2
  IF (.NOT. ASSOCIATED(Ptr, Tar2))                                            ERROR STOP 11
  IF (ANY( LBOUND(Ptr) .NE. (/1, 1 /)))                                       ERROR STOP 12
  IF (ANY( UBOUND(Ptr) .NE. (/N,N/)))                                         ERROR STOP 13
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=1,M)/),(/N,N/))))           ERROR STOP 14
  IF (ANY( Ptr%GName()  .NE. RESHAPE((/(CHAR(I), I=1,M)/),(/N,N/))))          ERROR STOP 15

  Ptr(0:9,0:9) => Tar1
  IF (.NOT. ASSOCIATED(Ptr))                                                  ERROR STOP 31
  IF (ANY( LBOUND(Ptr) .NE. (/0, 0 /)))                                       ERROR STOP 32
  IF (ANY( UBOUND(Ptr) .NE. (/9,9/)))                                         ERROR STOP 33
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=M,1,-1)/),(/N,N/))))        ERROR STOP 34
  IF (ANY( Ptr%GName(   )  .NE. RESHAPE((/(CHAR(I), I=M,1,-1)/),(/N,N/))))    ERROR STOP 35
  IF (ANY( GName(Ptr,Ptr)  .NE. RESHAPE((/(CHAR(I), I=M,1,-1)/),(/N,N/))))    ERROR STOP 36

  END

