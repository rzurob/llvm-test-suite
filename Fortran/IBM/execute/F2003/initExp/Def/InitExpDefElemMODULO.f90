!*********************************************************************
!*  ===================================================================
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
!*  -  MODULO
!*  (319212)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemMODULO
  IMPLICIT NONE
  INTEGER :: I, J



  TYPE :: DT0
    INTEGER(1)  :: I1=-8
    INTEGER(2)  :: I2=-8
    INTEGER(4)  :: I4=-8
    INTEGER(8)  :: I8=-8
    REAL(4)     :: R4=8.
    REAL(8)     :: R8=8.
    REAL(16)    :: R6=8.
  END TYPE

  TYPE (DT0), PARAMETER  :: T(128)=DT0()

  INTEGER(KIND(MODULO(P=T%I1, A=-5_1))), PARAMETER  :: TI1(128) = MODULO(P=T%I1, A=-5_1)
  INTEGER(KIND(MODULO(P=T%I2, A=-5_2))), PARAMETER  :: TI2(128) = MODULO(P=T%I2, A=-5_2)
  INTEGER(KIND(MODULO(P=T%I4, A=-5_4))), PARAMETER  :: TI4(128) = MODULO(P=T%I4, A=-5_4)
  INTEGER(KIND(MODULO(P=T%I8, A=-5_8))), PARAMETER  :: TI8(128) = MODULO(P=T%I8, A=-5_8)

  REAL(KIND(MODULO(P=T%R4, A=-5._4))),    PARAMETER  :: TR4(128) = MODULO(P=T%R4, A=-5._4)
  REAL(KIND(MODULO(P=T%R8, A=-5._8))),    PARAMETER  :: TR8(128) = MODULO(P=T%R8, A=-5._8)
  REAL(KIND(MODULO(P=T%R6, A=-5._16))),   PARAMETER  :: TR6(128) = MODULO(P=T%R6, A=-5._16)

  INTEGER :: Cnt1=COUNT(CSHIFT(MODULO(P=T%I2, A=-5_2), SHIFT=0)   .NE.  3)
  INTEGER :: Cnt2=COUNT(CSHIFT(MODULO(P=T%R8, A=-5._8), SHIFT=128) .NE. -2)


  IF ( KIND(TI1)  .NE.    1 ) ERROR STOP 11
  IF ( KIND(TI2)  .NE.    2 ) ERROR STOP 12
  IF ( KIND(TI4)  .NE.    4 ) ERROR STOP 13
  IF ( KIND(TI8)  .NE.    8 ) ERROR STOP 14
  IF ( KIND(TR4)  .NE.    4 ) ERROR STOP 15
  IF ( KIND(TR8)  .NE.    8 ) ERROR STOP 16
  IF ( KIND(TR6)  .NE.    16) ERROR STOP 17


  IF ( ANY (T%I1  .NE.  -8 )) ERROR STOP 21
  IF ( ANY (T%I2  .NE.  -8 )) ERROR STOP 22
  IF ( ANY (T%I4  .NE.  -8 )) ERROR STOP 23
  IF ( ANY (T%I8  .NE.  -8 )) ERROR STOP 24
  IF ( ANY (T%R4  .NE.   8 )) ERROR STOP 25
  IF ( ANY (T%R8  .NE.   8 )) ERROR STOP 26
  IF ( ANY (T%R6  .NE.   8 )) ERROR STOP 27

  IF ( Cnt1        .NE. 128 ) ERROR STOP 31
  IF ( Cnt2        .NE. 128 ) ERROR STOP 32

  END


