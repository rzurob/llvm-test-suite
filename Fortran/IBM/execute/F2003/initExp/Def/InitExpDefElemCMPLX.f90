!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemCMPLX.f
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
!*  - CMPLX
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemCMPLX
  IMPLICIT NONE
  INTEGER :: I, J, K

  COMPLEX,      PARAMETER :: Z=(4.0,4.0)

  INTEGER(1),   PARAMETER  :: CI1(8)=1
  INTEGER(2),   PARAMETER  :: CI2(8)=2
  INTEGER(4),   PARAMETER  :: CI4(8)=4
  INTEGER(8),   PARAMETER  :: CI8(8)=8

  REAL(4),   PARAMETER  :: CR4(8)=4.
  REAL(8),   PARAMETER  :: CR8(8)=8.
  REAL(16),  PARAMETER  :: CR16(8)=16.

  COMPLEX(KIND(CMPLX(Z, KIND=4))),   PARAMETER  :: Z4I(8) =CMPLX(CI1,  CI2,  4)
  COMPLEX(KIND(CMPLX(Z, KIND=8))),   PARAMETER  :: Z8I(8) =CMPLX(CI2,  CI4,  8)
  COMPLEX(KIND(CMPLX(Z, KIND=16))),  PARAMETER  :: Z16I(8)=CMPLX(CI1,  CI8, 16)

  COMPLEX(KIND(CMPLX(Z, KIND=4))),   PARAMETER  :: Z4IX(8) =CMPLX(CI1,   KIND=4)
  COMPLEX(KIND(CMPLX(Z, KIND=8))),   PARAMETER  :: Z8IX(8) =CMPLX(CI2,   KIND=8)
  COMPLEX(KIND(CMPLX(Z, KIND=16))),  PARAMETER  :: Z16IX(8)=CMPLX(CI1,   KIND=16)

  COMPLEX(KIND(CMPLX(Z, KIND=4))),   PARAMETER  :: Z4IXZ(8) =CMPLX(Z16I,  KIND=4)
  COMPLEX(KIND(CMPLX(Z, KIND=8))),   PARAMETER  :: Z8IXZ(8) =CMPLX(Z4I,   KIND=8)
  COMPLEX(KIND(CMPLX(Z, KIND=16))),  PARAMETER  :: Z16IXZ(8)=CMPLX(Z8I,   KIND=16)

  COMPLEX(KIND(CMPLX(Z, KIND=4))),   PARAMETER  :: Z4R(8) =CMPLX(CR8,  CR16,  4)
  COMPLEX(KIND(CMPLX(Z, KIND=8))),   PARAMETER  :: Z8R(8) =CMPLX(CR4,  CR16,  8)
  COMPLEX(KIND(CMPLX(Z, KIND=16))),  PARAMETER  :: Z16R(8)=CMPLX(CR4,  CR8, 16)

  COMPLEX(KIND(CMPLX(Z, KIND=4))),   PARAMETER  :: Z4RX(8) =CMPLX(CR8,   KIND=4)
  COMPLEX(KIND(CMPLX(Z, KIND=8))),   PARAMETER  :: Z8RX(8) =CMPLX(CR16,   KIND=8)
  COMPLEX(KIND(CMPLX(Z, KIND=16))),  PARAMETER  :: Z16RX(8)=CMPLX(CR4,   KIND=16)

  COMPLEX(KIND(CMPLX(Z, KIND=4))),   PARAMETER  :: Z4RXZ(8) =CMPLX(Z16R,  KIND=4)
  COMPLEX(KIND(CMPLX(Z, KIND=8))),   PARAMETER  :: Z8RXZ(8) =CMPLX(Z4R,   KIND=8)
  COMPLEX(KIND(CMPLX(Z, KIND=16))),  PARAMETER  :: Z16RXZ(8)=CMPLX(Z8R,   KIND=16)


  IF (KIND(Z4R)  .NE. 4 )                STOP 41
  IF (ANY( Z4R   .NE. (8.0_4, 16.0_4)))  STOP 42
  IF (KIND(Z8R)  .NE. 8 )                STOP 43
  IF (ANY( Z8R   .NE. (4.0_4, 16.0_4)))  STOP 44
  IF (KIND(Z16R) .NE. 16 )               STOP 45
  IF (ANY( Z16R  .NE. (4.0_4, 8.0_4)))   STOP 46

  IF (KIND(Z4RX)  .NE. 4 )               STOP 51
  IF (ANY( Z4RX   .NE. (8.0_4, 0.0_4)))  STOP 52
  IF (KIND(Z8RX)  .NE. 8 )               STOP 53
  IF (ANY( Z8RX   .NE. (16.0_4, 0.0_4))) STOP 54
  IF (KIND(Z16RX) .NE. 16 )              STOP 55
  IF (ANY( Z16RX  .NE. (4.0_4, 0.0_4)))  STOP 56

  IF (KIND(Z4RXZ)  .NE. 4 )              STOP 61
  IF (ANY( Z4RXZ   .NE. (4.0_4, 8.0_4))) STOP 62
  IF (KIND(Z8RXZ)  .NE. 8 )              STOP 63
  IF (ANY( Z8RXZ   .NE. (8.0_4, 16.0_4)))STOP 64
  IF (KIND(Z16RXZ) .NE. 16 )             STOP 65
  IF (ANY( Z16RXZ  .NE. (4.0_4, 16.0_4)))STOP 66


  IF (KIND(Z4I)  .NE. 4 )                STOP 11
  IF (ANY( Z4I   .NE. (1.0_4, 2.0_4)))   STOP 12
  IF (KIND(Z8I)  .NE. 8 )                STOP 13
  IF (ANY( Z8I   .NE. (2.0_4, 4.0_4)))   STOP 14
  IF (KIND(Z16I) .NE. 16 )               STOP 15
  IF (ANY( Z16I  .NE. (1.0_4, 8.0_4)))   STOP 16

  IF (KIND(Z4IX)  .NE. 4 )               STOP 21
  IF (ANY( Z4IX   .NE. (1.0_4, 0.0_4)))  STOP 22
  IF (KIND(Z8IX)  .NE. 8 )               STOP 23
  IF (ANY( Z8IX   .NE. (2.0_4, 0.0_4)))  STOP 24
  IF (KIND(Z16IX) .NE. 16 )              STOP 25
  IF (ANY( Z16IX  .NE. (1.0_4, 0.0_4)))  STOP 26

  IF (KIND(Z4IXZ)  .NE. 4 )              STOP 31
  IF (ANY( Z4IXZ   .NE. (1.0_4, 8.0_4))) STOP 32
  IF (KIND(Z8IXZ)  .NE. 8 )              STOP 33
  IF (ANY( Z8IXZ   .NE. (1.0_4, 2.0_4))) STOP 34
  IF (KIND(Z16IXZ) .NE. 16 )             STOP 35
  IF (ANY( Z16IXZ  .NE. (2.0_4, 4.0_4))) STOP 36









  END



