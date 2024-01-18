! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=self /tstdev/F2003/initExp/Misc/InitExpByte.f
! opt variations: -qck -qnok -qnol -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 28, 2006
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
!*  -- Byte
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpByte
  IMPLICIT NONE

  INTEGER :: I

  TYPE :: DT(K1,N1)    ! (1,1)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I1=1
    LOGICAL(K1)   :: L1=.TRUE.
    CHARACTER(N1) :: C=" "
  END TYPE

  TYPE(DT(1,1)), PARAMETER :: T=DT(1,1)()

  TYPE :: DT1(K2,N2)    ! (4,20)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    BYTE :: B1(T%I1%KIND)=(/(T%I1, I=1, T%I1%KIND)/)
    BYTE :: B2(T%L1%KIND)=(/(T%L1, I=1, T%L1%KIND)/)
    BYTE :: B3(T%C%KIND) =(/(T%C,  I=1, T%C%KIND)/)
  END TYPE

  TYPE(DT1(4,20)) :: T1


  IF (SIZE(T1%B1)    .NE. 1  ) STOP 11
  IF (SIZE(T1%B2)    .NE. 1  ) STOP 12
  IF (SIZE(T1%B3)    .NE. 1  ) STOP 13

  IF (ANY( T1%B1     .NE.   1))        STOP 61
  IF (ANY( T1%B2     .NEQV. .TRUE. ))  STOP 62
  IF (ANY( T1%B3     .NE.   " "))      STOP 63

  END


