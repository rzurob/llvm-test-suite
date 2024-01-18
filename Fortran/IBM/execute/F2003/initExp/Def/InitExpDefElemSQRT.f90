!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemSQRT.f
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
!*  -  SQRT
!*  (318967)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemSQRT
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(4), PARAMETER :: r4P_Zero        = REAL(z"00000000", KIND=4)
  REAL(4), PARAMETER :: r4P_1           = 1.0_4

  REAL(8), PARAMETER :: r8P_Zero        = REAL(z"0000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_1           = 1.0_8


  TYPE :: DT4
    REAL(4)  :: R41(4,4)=4
    REAL(4)  :: R42(4,4)=r4P_Zero
    REAL(4)  :: R43(4,4)=r4P_1
  END TYPE

  TYPE :: DT8
    REAL(8)  :: R81(4,4)=4
    REAL(8)  :: R82(4,4)=r8P_Zero
    REAL(8)  :: R83(4,4)=r8P_1
  END TYPE

  TYPE (DT4), PARAMETER :: X4 = DT4()
  TYPE (DT4)            :: T4 = DT4(                        &
                                     R41 = SQRT(X=X4%R41),  &
                                     R42 = SQRT(X=X4%R42),  &
                                     R43 = SQRT(X=X4%R43)   &
                                   )


  TYPE (DT8), PARAMETER :: X8 = DT8()
  TYPE (DT8)            :: T8 = DT8(                        &
                                     R81 = SQRT(X=X8%R81),  &
                                     R82 = SQRT(X=X8%R82),  &
                                     R83 = SQRT(X=X8%R83)   &
                                   )


  COMPLEX, PARAMETER :: Z4(4,4) = (1.0_4, .0_4)
  COMPLEX, PARAMETER :: Z8(4,4) = (1.0_8, .0_8)

  COMPLEX, PARAMETER :: TZ4(4,4) = SQRT(X=Z4)
  COMPLEX, PARAMETER :: TZ8(4,4) = SQRT(X=Z8)


  IF (ANY( T4%R41   .NE. 2.  ))       STOP 11
  IF (ANY( T4%R42   .NE. r4P_Zero  )) STOP 12
  IF (ANY( T4%R43   .NE. 1  ))        STOP 13

  IF (ANY( T8%R81   .NE. 2.  ))       STOP 21
  IF (ANY( T8%R82   .NE. r8P_Zero  )) STOP 22
  IF (ANY( T8%R83   .NE. 1  ))        STOP 23

  IF (ANY( TZ4      .NE. Z4 ))        STOP 31
  IF (ANY( TZ8      .NE. Z8 ))        STOP 32



  END


