!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 27, 2006
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
!*  a reference to a transformational intrinsic
!*
!*  - ANY
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemANY
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(4),   PARAMETER :: B4(2,3)=RESHAPE((/1.,2.,3.,4.,5.,6./),(/2,3/))
  REAL(8),   PARAMETER :: B8(2,3)=B4
  REAL(16),  PARAMETER :: B16(2,3)=B8

  REAL(4),   PARAMETER :: C4(2,3)=RESHAPE((/0.,7.,3.,4.,5.,8./),(/2,3/))
  REAL(8),   PARAMETER :: C8(2,3)=C4
  REAL(16),  PARAMETER :: C16(2,3)=C8

  LOGICAL,      PARAMETER :: R1(3) =RESHAPE((/.TRUE.,  .FALSE., .TRUE./),(/3/))
  LOGICAL,      PARAMETER :: R3(2) =RESHAPE((/.TRUE.,  .TRUE./),(/2/))
  LOGICAL(1),   PARAMETER :: R21(2)=RESHAPE((/.TRUE.,  .TRUE./),(/2/))
  LOGICAL(2),   PARAMETER :: R22(2)=R21
  LOGICAL(4),   PARAMETER :: R24(2)=R22
  LOGICAL(8),   PARAMETER :: R28(2)=R24

  LOGICAL(KIND(ANY(R21))) :: T11=ANY(R21)
  LOGICAL(KIND(ANY(R22))) :: T12=ANY(R22)
  LOGICAL(KIND(ANY(R24))) :: T14=ANY(R24 .EQV. R24)
  LOGICAL(KIND(ANY(R28))) :: T18=ANY(R28 .NEQV. R21)

  LOGICAL(1), PARAMETER   :: Z1(1:0, 2)= .FALSE.
  LOGICAL(2), PARAMETER   :: Z2(1:0, 2)= .FALSE.
  LOGICAL(4), PARAMETER   :: Z4(1:0, 2)= .FALSE.
  LOGICAL(8), PARAMETER   :: Z8(1:0, 2)= .FALSE.

  LOGICAL(KIND(ANY(Z1))), PARAMETER  :: T21(3)=ANY(Z1)
  LOGICAL(KIND(ANY(Z2))), PARAMETER  :: T22(3)=ANY(Z2)
  LOGICAL(KIND(ANY(Z4))), PARAMETER  :: T24(3)=ANY(Z4)
  LOGICAL(KIND(ANY(Z8))), PARAMETER  :: T28(3)=ANY(Z8)

  LOGICAL(KIND(ANY(T21))), PARAMETER :: T31(3)=ANY(LOGICAL(B4  .NE. C8, KIND=1), 1)
  LOGICAL(KIND(ANY(T22))), PARAMETER :: T32(3)=ANY(LOGICAL(B16 .NE. C4, KIND=2), 1)
  LOGICAL(KIND(ANY(T24))), PARAMETER  :: T34(3)=ANY((B4 .NE. C4), 1)
  LOGICAL(KIND(ANY(T28))), PARAMETER :: T38(3)=ANY((B8 .NE. C4), 1)

  LOGICAL(KIND(ANY(T31))) :: T41(2)=ANY(LOGICAL(B4 .NE. C8,  KIND=1), DIM=2)
  LOGICAL(KIND(ANY(T32))) :: T42(2)=ANY(LOGICAL(B8 .NE. C16, KIND=2), DIM=2)
  LOGICAL(KIND(ANY(T34))) :: T44(2)=ANY((B4 .NE. C4), DIM=2)
  LOGICAL(KIND(ANY(T38))) :: T48(2)=ANY((B4 .NE. C8), DIM=2)


  IF (KIND(T11)   .NE.   1 )        STOP 11
  IF (            .NOT.  T11 )      STOP 12
  IF (KIND(T12)   .NE.   2 )        STOP 13
  IF (            .NOT.  T12 )      STOP 14
  IF (KIND(T14)   .NE.   4 )        STOP 15
  IF (            .NOT.  T14 )      STOP 16
  IF (KIND(T18)   .NE.   8 )        STOP 17
  IF (                   T18 )      STOP 18

  IF (KIND(T21)   .NE.   1 )        STOP 21
  IF ( ANY(T21    .NEQV. .FALSE. )) STOP 22
  IF (KIND(T22)   .NE.   2 )        STOP 23
  IF ( ANY(T22    .NEQV. .FALSE. )) STOP 24
  IF (KIND(T24)   .NE.   4 )        STOP 25
  IF ( ANY(T24    .NEQV. .FALSE. )) STOP 26
  IF (KIND(T28)   .NE.   8 )        STOP 27
  IF ( ANY(T28    .NEQV. .FALSE. )) STOP 28

  IF (KIND(T31)  .NE.   1 )     STOP 31
  IF ( ANY( T31  .NEQV. R1) )   STOP 32
  IF (KIND(T32)  .NE.   2 )     STOP 33
  IF ( ANY( T32  .NEQV. R1) )   STOP 34
  IF (KIND(T34)  .NE.   4 )     STOP 35
  IF ( ANY( T34  .NEQV. R1) )   STOP 36
  IF (KIND(T38)  .NE.   8 )     STOP 37
  IF ( ANY( T38  .NEQV. R1) )   STOP 38

  IF (KIND(T41)  .NE.   1 )     STOP 41
  IF ( ANY( T41  .NEQV. R3) )   STOP 42
  IF (KIND(T42)  .NE.   2 )     STOP 43
  IF ( ANY( T42  .NEQV. R3) )   STOP 44
  IF (KIND(T44)  .NE.   4 )     STOP 45
  IF ( ANY( T44  .NEQV. R3) )   STOP 46
  IF (KIND(T48)  .NE.   8 )     STOP 47
  IF ( ANY( T48  .NEQV. R3) )   STOP 48



  END



