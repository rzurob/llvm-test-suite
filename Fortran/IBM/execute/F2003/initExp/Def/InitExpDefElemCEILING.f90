!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 24, 2006
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
!*  - CEILING
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemCEILING
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(CEILING(3.11)), PARAMETER :: R4(CEILING(3.11), CEILING(3.101))=    &
                     RESHAPE(CEILING((/(I-0.01, I=1,16)/)),(/CEILING(3.11), CEILING(3.101)/))

  REAL(CEILING(7.11_4)), PARAMETER :: R8(CEILING(3.11), CEILING(3.101))=    &
                     RESHAPE(CEILING((/(I-0.04*I, I=1,16)/)),(/CEILING(3.81), CEILING(3.991)/))

  REAL(16), PARAMETER :: R16(CEILING(3.999), CEILING(3.99))=    &
                     RESHAPE(CEILING(R8+1, KIND=4), (/4, INT(R8(4,1))/) )

  TYPE :: DT

    REAL(CEILING(3.1))  :: R44(CEILING(3.01), CEILING(3.001)) = CEILING(R4(:,:))
    REAL(CEILING(3.1))  :: R48(CEILING(3.01), CEILING(3.001)) = CEILING(R4(:,:)+R8)
    REAL(CEILING(3.1))  :: R416(CEILING(3.01), CEILING(3.001)) = CEILING(R16-R4)

    REAL(CEILING(7.1))  :: R84(CEILING(3.01), CEILING(3.001)) = CEILING(R4(:,:))
    REAL(CEILING(7.1))  :: R88(CEILING(3.01), CEILING(3.001)) = CEILING(R4(:,:)+R8)
    REAL(CEILING(7.1))  :: R816(CEILING(3.01), CEILING(3.001)) = CEILING(R16-R4)

    REAL(CEILING(15.1))  :: R164(CEILING(3.01), CEILING(3.0001)) = CEILING(R4(:,:))
    REAL(CEILING(15.1))  :: R168(CEILING(3.01), CEILING(3.0001)) = CEILING(R4(:,:)+R8)
    REAL(CEILING(15.1))  :: R1616(CEILING(3.01), CEILING(3.0001)) = CEILING(R16-R4)

  END TYPE

  TYPE(DT) :: T

  IF (ANY( R4  .NE. RESHAPE((/(I, I=1,16)/),(/4,4/) ) ) )  ERROR STOP 11
  IF (ANY( R8  .NE. RESHAPE((/(I, I=1,16)/),(/4,4/) ) ) )  ERROR STOP 12
  IF (ANY( R16 .NE. RESHAPE((/(I, I=2,17)/),(/4,4/) ) ) )  ERROR STOP 13

  IF (KIND(T%R44) .NE. 4 )         ERROR STOP 21
  IF (ANY( T%R44  .NE. R4 ) )      ERROR STOP 22
  IF (ANY( T%R48  .NE. R4+R8 ) )   ERROR STOP 23
  IF (ANY( T%R416 .NE. R16-R4 ) )  ERROR STOP 24

  IF (KIND(T%R84) .NE. 8 )         ERROR STOP 31
  IF (ANY( T%R84  .NE. R4 ) )      ERROR STOP 32
  IF (ANY( T%R88  .NE. R4+R8 ) )   ERROR STOP 33
  IF (ANY( T%R816 .NE. R16-R4 ) )  ERROR STOP 34

  IF (KIND(T%R164) .NE. 16 )        ERROR STOP 41
  IF (ANY( T%R164  .NE. R4 ) )      ERROR STOP 42
  IF (ANY( T%R168  .NE. R4+R8 ) )   ERROR STOP 43
  IF (ANY( T%R1616 .NE. R16-R4 ) )  ERROR STOP 44


  END


