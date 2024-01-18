!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemBTEST.f
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
!*  - BTEST
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemBTEST
  IMPLICIT NONE
  INTEGER :: I, J, K

  LOGICAL(4), PARAMETER    :: L1=BTEST(8_1,3_2)
  LOGICAL(4), PARAMETER    :: L2=BTEST(8_2,3_8)
  LOGICAL(4), PARAMETER    :: L4=BTEST(8_4,3_1)
  LOGICAL(4), PARAMETER    :: L8=BTEST(8_8,3_4)

  INTEGER(1), PARAMETER    :: A1(2,2)=RESHAPE((/1,3,2,4/),(/2,2/))
  INTEGER(2), PARAMETER    :: A2(2,2)=RESHAPE((/1,3,2,4/),(/2,2/))
  INTEGER(4), PARAMETER    :: A4(2,2)=RESHAPE((/1,3,2,4/),(/2,2/))
  INTEGER(8), PARAMETER    :: A8(2,2)=RESHAPE((/1,3,2,4/),(/2,2/))

  LOGICAL(4), PARAMETER    :: R1(2,2)=RESHAPE((/.FALSE.,.FALSE.,.FALSE.,.TRUE./),(/2,2/))
  LOGICAL(4), PARAMETER    :: R2(2,2)=RESHAPE((/.TRUE.,.FALSE.,.FALSE.,.FALSE./),(/2,2/))

  LOGICAL(4), PARAMETER    :: T11(2,2)=BTEST(A1, 2_1)
  LOGICAL(4), PARAMETER    :: T12(2,2)=BTEST(A1, 2_2)
  LOGICAL(4), PARAMETER    :: T14(2,2)=BTEST(A1, 2_4)
  LOGICAL(4), PARAMETER    :: T18(2,2)=BTEST(A1, 2_8)

  LOGICAL(4), PARAMETER    :: T21(2,2)=BTEST(A2, 2_1)
  LOGICAL(4), PARAMETER    :: T22(2,2)=BTEST(A2, 2_2)
  LOGICAL(4), PARAMETER    :: T24(2,2)=BTEST(A2, 2_4)
  LOGICAL(4), PARAMETER    :: T28(2,2)=BTEST(A2, 2_8)

  LOGICAL(4), PARAMETER    :: T41(2,2)=BTEST(A4, 2_1)
  LOGICAL(4), PARAMETER    :: T42(2,2)=BTEST(A4, 2_2)
  LOGICAL(4), PARAMETER    :: T44(2,2)=BTEST(A4, 2_4)
  LOGICAL(4), PARAMETER    :: T48(2,2)=BTEST(A4, 2_8)

  LOGICAL(4), PARAMETER    :: T81(2,2)=BTEST(A8, 2_1)
  LOGICAL(4), PARAMETER    :: T82(2,2)=BTEST(A8, 2_2)
  LOGICAL(4), PARAMETER    :: T84(2,2)=BTEST(A8, 2_4)
  LOGICAL(4), PARAMETER    :: T88(2,2)=BTEST(A8, 2_8)

  LOGICAL(4), PARAMETER    :: S11(2,2)=BTEST(2_1, A1)
  LOGICAL(4), PARAMETER    :: S12(2,2)=BTEST(2_2, A1)
  LOGICAL(4), PARAMETER    :: S14(2,2)=BTEST(2_4, A1)
  LOGICAL(4), PARAMETER    :: S18(2,2)=BTEST(2_8, A1)

  LOGICAL(4), PARAMETER    :: S21(2,2)=BTEST(2_1, A2)
  LOGICAL(4), PARAMETER    :: S22(2,2)=BTEST(2_2, A2)
  LOGICAL(4), PARAMETER    :: S24(2,2)=BTEST(2_4, A2)
  LOGICAL(4), PARAMETER    :: S28(2,2)=BTEST(2_8, A2)

  LOGICAL(4), PARAMETER    :: S41(2,2)=BTEST(2_1, A4)
  LOGICAL(4), PARAMETER    :: S42(2,2)=BTEST(2_2, A4)
  LOGICAL(4), PARAMETER    :: S44(2,2)=BTEST(2_4, A4)
  LOGICAL(4), PARAMETER    :: S48(2,2)=BTEST(2_8, A4)

  LOGICAL(4), PARAMETER    :: S81(2,2)=BTEST(2_1, A8)
  LOGICAL(4), PARAMETER    :: S82(2,2)=BTEST(2_2, A8)
  LOGICAL(4), PARAMETER    :: S84(2,2)=BTEST(2_4, A8)
  LOGICAL(4), PARAMETER    :: S88(2,2)=BTEST(2_8, A8)



  IF ( .NOT. L1 )      STOP 11
  IF ( .NOT. L2 )      STOP 12
  IF ( .NOT. L4 )      STOP 13
  IF ( .NOT. L8 )      STOP 14

  IF (ANY( T11 .NEQV. R1  ) )    STOP 21
  IF (ANY( T12 .NEQV. R1  ) )    STOP 22
  IF (ANY( T14 .NEQV. R1  ) )    STOP 23
  IF (ANY( T18 .NEQV. R1  ) )    STOP 24

  IF (ANY( T21 .NEQV. R1  ) )    STOP 31
  IF (ANY( T22 .NEQV. R1  ) )    STOP 32
  IF (ANY( T24 .NEQV. R1  ) )    STOP 33
  IF (ANY( T28 .NEQV. R1  ) )    STOP 34

  IF (ANY( T41 .NEQV. R1  ) )    STOP 41
  IF (ANY( T42 .NEQV. R1  ) )    STOP 42
  IF (ANY( T44 .NEQV. R1  ) )    STOP 43
  IF (ANY( T48 .NEQV. R1  ) )    STOP 44

  IF (ANY( T81 .NEQV. R1  ) )    STOP 51
  IF (ANY( T82 .NEQV. R1  ) )    STOP 52
  IF (ANY( T84 .NEQV. R1  ) )    STOP 53
  IF (ANY( T88 .NEQV. R1  ) )    STOP 54

  IF (ANY( S11 .NEQV. R2  ) )    STOP 61
  IF (ANY( S12 .NEQV. R2  ) )    STOP 62
  IF (ANY( S14 .NEQV. R2  ) )    STOP 63
  IF (ANY( S18 .NEQV. R2  ) )    STOP 64

  IF (ANY( S21 .NEQV. R2  ) )    STOP 71
  IF (ANY( S22 .NEQV. R2  ) )    STOP 72
  IF (ANY( S24 .NEQV. R2  ) )    STOP 73
  IF (ANY( s28 .NEQV. R2  ) )    STOP 74

  IF (ANY( S41 .NEQV. R2  ) )    STOP 81
  IF (ANY( S42 .NEQV. R2  ) )    STOP 82
  IF (ANY( S44 .NEQV. R2  ) )    STOP 83
  IF (ANY( S48 .NEQV. R2  ) )    STOP 84

  IF (ANY( S81 .NEQV. R2  ) )    STOP 91
  IF (ANY( S82 .NEQV. R2  ) )    STOP 92
  IF (ANY( S84 .NEQV. R2  ) )    STOP 93
  IF (ANY( S88 .NEQV. R2  ) )    STOP 94

  END


