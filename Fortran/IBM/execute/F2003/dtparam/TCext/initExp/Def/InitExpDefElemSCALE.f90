! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/initExp/Def/InitExpDefElemSCALE.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 14, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an elemental intrinsic
!*
!*  -  SCALE
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemSCALE
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(4), PARAMETER :: r4Quiet_N_NaN     = REAL(z"FFFFFFFF", KIND=4)
  REAL(4), PARAMETER :: r4Max_N           = REAL(z"FF7FFFFF", KIND=4)
  REAL(4), PARAMETER :: r4Min_N           = REAL(z"80800000", KIND=4)
  REAL(4), PARAMETER :: r4N_Zero          = REAL(z"80000000", KIND=4)
  REAL(4), PARAMETER :: r4P_Zero          = REAL(z"00000000", KIND=4)
  REAL(4), PARAMETER :: r4Min_P           = REAL(z"00800000", KIND=4)
  REAL(4), PARAMETER :: r4Max_P           = REAL(z"7F7FFFFF", KIND=4)
  REAL(4), PARAMETER :: r4Quiet_P_NaN     = REAL(z"7FFFFFFF", KIND=4)


  REAL(8), PARAMETER :: r8Quiet_N_NaN     = REAL(z"FFFFFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8Max_N           = REAL(z"FF7FFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8Min_N           = REAL(z"8080000000000000", KIND=8)
  REAL(8), PARAMETER :: r8N_Zero          = REAL(z"8000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_Zero          = REAL(z"0000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8Min_P           = REAL(z"0080000000000000", KIND=8)
  REAL(8), PARAMETER :: r8Max_P           = REAL(z"7F7FFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8Quiet_P_NaN     = REAL(z"7FFFFFFFFFFFFFFF", KIND=8)



  TYPE :: DT(K1,K2,K3)    ! (4,8,16)
    INTEGER, KIND :: K1,K2,K3
    REAL(K1)      :: R4(4,4)=RESHAPE((/(3.0, I=1,16)/),(/4,4/))
    REAL(K2)      :: R8(4,4)=RESHAPE((/(3.0, I=1,16)/),(/4,4/))
    REAL(K3)      :: R6(4,4)=RESHAPE((/(3.0, I=1,16)/),(/4,4/))
  END TYPE

  TYPE (DT(4,8,16)), PARAMETER :: X = DT(4,8,16)()
  REAL(4), PARAMETER :: RR=12.

  REAL(KIND(RESHAPE(SOURCE=SCALE(X=X%R4, I=2_1),SHAPE=(/4,4/))))  ::   &
   TR4(4,4)=RESHAPE(SOURCE=SCALE(X=X%R4, I=2_1),SHAPE=(/4,4/))

  REAL(KIND(RESHAPE(SOURCE=SCALE(X=X%R8, I=2_2),SHAPE=(/4,4/))))  ::   &
   TR8(4,4)=RESHAPE(SOURCE=SCALE(X=X%R8, I=2_2),SHAPE=(/4,4/))

  REAL(KIND(RESHAPE(SOURCE=SCALE(X=X%R6, I=2_8),SHAPE=(/4,4/))))  ::   &
   TR6(4,4)=RESHAPE(SOURCE=SCALE(X=X%R6, I=2_8),SHAPE=(/4,4/))

  REAL(4) :: T41=SCALE(X=r4Max_N, I=-1)
  REAL(4) :: T42=SCALE(X=r4Min_N, I=-1 )
  REAL(4) :: T43=SCALE(X=r4N_Zero, I=-1 )
  REAL(4) :: T44=SCALE(X=r4P_Zero, I=1 )
  REAL(4) :: T45=SCALE(X=r4Min_P, I=-1)
  REAL(4) :: T46=SCALE(X=r4Max_P, I=1)

  REAL(8) :: T81=SCALE(X=r8Max_N, I=-1)
  REAL(8) :: T82=SCALE(X=r8Min_N, I=-1 )
  REAL(8) :: T83=SCALE(X=r8N_Zero, I=-1 )
  REAL(8) :: T84=SCALE(X=r8P_Zero, I=1 )
  REAL(8) :: T85=SCALE(X=r8Min_P, I=-1)
  REAL(8) :: T86=SCALE(X=r8Max_P, I=1)



  IF ( KIND(TR4)  .NE.    4 )  ERROR STOP 11
  IF ( KIND(TR8)  .NE.    8 )  ERROR STOP 12
  IF ( KIND(TR6)  .NE.   16 )  ERROR STOP 13

  IF (ANY( TR4  .NE. RR  )) ERROR STOP 21
  IF (ANY( TR8  .NE. RR  )) ERROR STOP 22
  IF (ANY( TR6  .NE. RR  )) ERROR STOP 23

! IF ( T41  .NE.  r4Quiet_N_NaN )      ERROR STOP 31   ! processor dependent
  IF ( T42  .NE.  r4Min_N/2.0 )        ERROR STOP 32
  IF ( T43  .NE.  r4N_Zero )           ERROR STOP 33
  IF ( T44  .NE.  r4P_Zero )           ERROR STOP 34
  IF ( T43  .NE.  r4P_Zero )           ERROR STOP 35
! IF ( T44  .NE.  r4Quiet_P_NaN )      ERROR STOP 36

! IF ( T81  .NE.  r8Quiet_N_NaN )      ERROR STOP 41
  IF ( T82  .NE.  r8Min_N/2. )         ERROR STOP 42
  IF ( T83  .NE.  r8N_Zero )           ERROR STOP 43
  IF ( T84  .NE.  r8P_Zero )           ERROR STOP 44
  IF ( T83  .NE.  r8P_Zero )           ERROR STOP 45
! IF ( T84  .NE.  r8Quiet_P_NaN )      ERROR STOP 46


  END


