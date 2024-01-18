! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/InitExpDefElemMOD.f
! opt variations: -qnol -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemMOD.f
!*
!*  DATE                       : Apr. 12, 2006
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
!*  -  MOD
!*  (319209)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemMOD
  IMPLICIT NONE
  INTEGER :: I, J



  TYPE :: DT0(N1,K1,K2,K3,K4,K5,K6,K7)    ! (20,1,2,4,8,4,8,16)
    INTEGER, KIND :: K1,K2,K3,K4,K5,K6,K7
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I1=-8
    INTEGER(K2)   :: I2=-8
    INTEGER(K3)   :: I4=-8
    INTEGER(K4)   :: I8=-8
    REAL(K5)      :: R4=8.
    REAL(K6)      :: R8=8.
    REAL(K7)      :: R6=8.
  END TYPE

  TYPE (DT0(20,1,2,4,8,4,8,16)), PARAMETER  :: T(128)=DT0(20,1,2,4,8,4,8,16)()

  INTEGER(KIND(MOD(P=T%I1, A=-5_1))), PARAMETER  :: TI1(128) = MOD(P=T%I1, A=-5_1)
  INTEGER(KIND(MOD(P=T%I2, A=-5_2))), PARAMETER  :: TI2(128) = MOD(P=T%I2, A=-5_2)
  INTEGER(KIND(MOD(P=T%I4, A=-5_4))), PARAMETER  :: TI4(128) = MOD(P=T%I4, A=-5_4)
  INTEGER(KIND(MOD(P=T%I8, A=-5_8))), PARAMETER  :: TI8(128) = MOD(P=T%I8, A=-5_8)

  REAL(KIND(MOD(P=T%R4, A=-5._4))),    PARAMETER  :: TR4(128) = MOD(P=T%R4, A=-5._4)
  REAL(KIND(MOD(P=T%R8, A=-5._8))),    PARAMETER  :: TR8(128) = MOD(P=T%R8, A=-5._8)
  REAL(KIND(MOD(P=T%R6, A=-5._16))),   PARAMETER  :: TR6(128) = MOD(P=T%R6, A=-5._16)

  INTEGER :: Cnt1=COUNT(MOD(P=T%I8, A=-5_8) .NE. -3)
  INTEGER :: Cnt2=COUNT(MOD(P=T%R6, A=-5._16) .NE.  3)


  IF ( KIND(TI1)  .NE.    1 ) STOP 11
  IF ( KIND(TI2)  .NE.    2 ) STOP 12
  IF ( KIND(TI4)  .NE.    4 ) STOP 13
  IF ( KIND(TI8)  .NE.    8 ) STOP 14
  IF ( KIND(TR4)  .NE.    4 ) STOP 15
  IF ( KIND(TR8)  .NE.    8 ) STOP 16
  IF ( KIND(TR6)  .NE.    16) STOP 17


  IF ( ANY (T%I1   .NE.  -8 )) STOP 21
  IF ( ANY (T%I2   .NE.  -8 )) STOP 22
  IF ( ANY (T%I4   .NE.  -8 )) STOP 23
  IF ( ANY (T%I8   .NE.  -8 )) STOP 24
  IF ( ANY (T%R4   .NE.   8 )) STOP 25
  IF ( ANY (T%R8   .NE.   8 )) STOP 26
  IF ( ANY (T%R6   .NE.   8 )) STOP 27

  IF ( Cnt1        .NE. 128 ) STOP 31
  IF ( Cnt2        .NE. 128 ) STOP 32

  END


