! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp /tstdev/F2003/dataPtrAssgn/Misc/dataPtrTypBndDefOp.f
! opt variations: -qck -qnok -qnodeferredlp

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
!*  interaction with type bound generics
!
!*  -- Defined Operator
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE Mod

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
  CONTAINS
    GENERIC     :: OPERATOR( + ) => ModFun
    PROCEDURE   :: ModFun
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4,20)
  END TYPE

  INTERFACE OPERATOR( + )
    PROCEDURE ModFun   ! can be dup
  END INTERFACE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg1, Arg2)
  CLASS(DT(4,*)), INTENT(IN) :: Arg1
  TYPE(DT(4,*)), INTENT(IN) :: Arg2
  TYPE(DT(4,20))             :: ModFun
    ModFun%ID = "ModFun-"// TRIM(Arg1%ID) // TRIM(Arg2%ID)
  END FUNCTION

  END MODULE


  PROGRAM dataPtrTypBndDefOp
  USE Mod


  INTEGER            :: I, J, K
  INTEGER, PARAMETER :: N=10, M=100

  TYPE(DT(4,20)), TARGET  :: Tar1(M)=(/(DT(4,20)(ID=CHAR(I)), I=M,1,-1)/)
  TYPE(DT(4,20)), TARGET  :: Tar2(N,N)=RESHAPE((/(DT(4,20)(ID=CHAR(I)), I=1,M)/),(/N,N/))
  TYPE(DT(4,:)),POINTER :: Ptr(:, :)


  Ptr(1:,1:) => Tar2
  IF (.NOT. ASSOCIATED(Ptr, Tar2))                                            ERROR STOP 21
  IF (ANY( LBOUND(Ptr) .NE. (/1, 1 /)))                                       ERROR STOP 22
  IF (ANY( UBOUND(Ptr) .NE. (/N,N/)))                                         ERROR STOP 23
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=1,M)/),(/N,N/))))           ERROR STOP 24

  Ptr = Ptr + Ptr
  IF (ANY( Ptr%ID  .NE. RESHAPE((/("ModFun-" //CHAR(I)//CHAR(I), I=1,M)/),(/N,N/))))  ERROR STOP 25

  Ptr(0:9,0:9) => Tar1
  IF (.NOT. ASSOCIATED(Ptr))                                                  ERROR STOP 31
  IF (ANY( LBOUND(Ptr) .NE. (/0, 0 /)))                                       ERROR STOP 32
  IF (ANY( UBOUND(Ptr) .NE. (/9,9/)))                                         ERROR STOP 33
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=M,1,-1)/),(/N,N/))))        ERROR STOP 34

  Ptr = Ptr + Ptr
  IF (ANY( Ptr%ID  .NE. RESHAPE((/("ModFun-" //CHAR(I)//CHAR(I), I=M,1,-1)/),(/N,N/))))  ERROR STOP 35
  IF (ANY( Tar1%ID  .NE. (/("ModFun-" //CHAR(I)//CHAR(I), I=M,1,-1)/)))                  ERROR STOP 36


  END


