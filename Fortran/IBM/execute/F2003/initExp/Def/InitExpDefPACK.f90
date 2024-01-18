!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 29, 2006
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
!*  a reference to an tranformational intrinsic
!*
!*  - PACK
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    CHARACTER  :: C=" "
    LOGICAL(2) :: L(3,3)=.FALSE.
    PROCEDURE(), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE

  PROGRAM   InitExpDefPACK
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),         PARAMETER :: I11(6) =(/(I, I=1,6)/)
  LOGICAL(1),         PARAMETER :: M11(6) =(/(.TRUE.,.FALSE., I=1,3)/)
  INTEGER(1),         PARAMETER :: V11(6) =(/(-I, I=1,6)/)
  INTEGER(1),         PARAMETER :: PI11(6)=(/(/1,3,5/),(/-4,-5,-6/)/)

  INTEGER(KIND(PACK(I11, .TRUE.)))   :: TI11(6)=PACK(I11, .TRUE.)
  INTEGER(KIND(PACK(I11, M11)))      :: TI12(3)=PACK(I11, M11)
  INTEGER(KIND(PACK(I11, M11, V11))) :: TI13(6)=PACK(I11, MASK=M11, VECTOR=V11)

  INTEGER(4),         PARAMETER :: I41(6) =(/(I, I=1,6)/)
  LOGICAL(4),         PARAMETER :: M41(6) =(/(.TRUE.,.FALSE., I=1,3)/)
  INTEGER(4),         PARAMETER :: V41(6) =(/(-I, I=1,6)/)
  INTEGER(4),         PARAMETER :: PI41(6)=(/(/1,3,5/),(/-4,-5,-6/)/)

  INTEGER(KIND(PACK(I41, .TRUE.)))   :: TI41(6)=PACK(I41, .TRUE.)
  INTEGER(KIND(PACK(I41, M41)))      :: TI42(3)=PACK(I41, M41)
  INTEGER(KIND(PACK(I41, M41, V41))) :: TI43(6)=PACK(I41, MASK=M41, VECTOR=V41)

  INTEGER(8),         PARAMETER :: I81(6) =(/(I, I=1,6)/)
  LOGICAL(8),         PARAMETER :: M81(6) =(/(.TRUE.,.FALSE., I=1,3)/)
  INTEGER(8),         PARAMETER :: V81(6) =(/(-I, I=1,6)/)
  INTEGER(8),         PARAMETER :: PI81(6)=(/(/1,3,5/),(/-4,-5,-6/)/)

  INTEGER(KIND(PACK(I81, .TRUE.)))   :: TI81(6)=PACK(I81, .TRUE.)
  INTEGER(KIND(PACK(I81, M81)))      :: TI82(3)=PACK(I81, M81)
  INTEGER(KIND(PACK(I81, M81, V81))) :: TI83(6)=PACK(I81, MASK=M41, VECTOR=V81)

  REAL(8),         PARAMETER :: R81(6) =(/(I, I=1,6)/)
  REAL(8),         PARAMETER :: RV81(6) =(/(-I, I=1,6)/)

  REAL(KIND(PACK(R81, .TRUE.)))   :: TR81(6)=PACK(R81, .TRUE.)
  REAL(KIND(PACK(R81, M11)))      :: TR82(3)=PACK(R81, M41)
  REAL(KIND(PACK(R81, M11, RV81))) :: TR83(6)=PACK(R81, MASK=M81, VECTOR=RV81)

  COMPLEX(4),         PARAMETER :: Z41(6) =(/((I,-I), I=1,6)/)
  COMPLEX(4),         PARAMETER :: ZV41(6)=(/((-I,I), I=1,6)/)
  COMPLEX(4),         PARAMETER :: PZ41(6)=(/(/(1,-1),(3,-3),(5,-5)/),(/(-4,4),(-5,5),(-6,6)/)/)

  COMPLEX(KIND(PACK(Z41, .TRUE.)))   :: TZ41(6)=PACK(Z41, .TRUE.)
  COMPLEX(KIND(PACK(Z41, M11)))      :: TZ42(3)=PACK(Z41, M11)
  COMPLEX(KIND(PACK(Z41, M41, ZV41))) :: TZ43(6)=PACK(Z41, MASK=M41, VECTOR=ZV41)

  TYPE(DT),  PARAMETER :: T(6) =(/(DT(CHAR(I)), I=1,6)/)
  TYPE(DT),  PARAMETER :: V(6) =(/(DT(CHAR(I)), I=7,12)/)
  TYPE(DT),  pARAMETER :: P(6) =(/DT(CHAR(1)),DT(CHAR(3)),DT(CHAR(5)),DT(CHAR(10)),DT(CHAR(11)),DT(CHAR(12))/)

  TYPE(DT) :: T1(6)=PACK(T, .TRUE.)
  TYPE(DT) :: T2(3)=PACK(T, M11)
  TYPE(DT) :: T3(6)=PACK(T, MASK=M41, VECTOR=V)



  IF (KIND(TI11)   .NE.   1 )                STOP 11
  IF (ANY( TI11    .NE.  (/(I, I=1,6)/)))    STOP 12
  IF (KIND(TI12)   .NE.   1 )                STOP 13
  IF (ANY( TI12    .NE.  (/(I, I=1,6,2)/)))  STOP 14
  IF (KIND(TI13)   .NE.   1 )                STOP 15
  IF (ANY( TI13    .NE.  PI11))              STOP 16

  IF (KIND(TI41)   .NE.   4 )                STOP 21
  IF (ANY( TI41    .NE.  (/(I, I=1,6)/)))    STOP 22
  IF (KIND(TI42)   .NE.   4 )                STOP 23
  IF (ANY( TI42    .NE.  (/(I, I=1,6,2)/)))  STOP 24
  IF (KIND(TI43)   .NE.   4 )                STOP 25
  IF (ANY( TI43    .NE.  PI41))              STOP 26

  IF (KIND(TI81)   .NE.   8 )                STOP 31
  IF (ANY( TI81    .NE.  (/(I, I=1,6)/)))    STOP 32
  IF (KIND(TI82)   .NE.   8 )                STOP 33
  IF (ANY( TI82    .NE.  (/(I, I=1,6,2)/)))  STOP 34
  IF (KIND(TI83)   .NE.   8 )                STOP 35
  IF (ANY( TI83    .NE.  PI81))              STOP 36

  IF (KIND(TR81)   .NE.   8 )                STOP 41
  IF (ANY( TR81    .NE.  (/(I, I=1,6)/)))    STOP 42
  IF (KIND(TR82)   .NE.   8 )                STOP 43
  IF (ANY( TR82    .NE.  (/(I, I=1,6,2)/)))  STOP 44
  IF (KIND(TR83)   .NE.   8 )                STOP 45
  IF (ANY( TR83    .NE.  PI81))              STOP 46

  IF (KIND(TZ41)   .NE.   4 )                     STOP 51
  IF (ANY( TZ41    .NE.  (/((I,-I), I=1,6)/)))    STOP 52
  IF (KIND(TZ42)   .NE.   4 )                     STOP 53
  IF (ANY( TZ42    .NE.  (/((I,-I), I=1,6,2)/)))  STOP 54
  IF (KIND(TZ43)   .NE.   4 )                     STOP 55
  IF (ANY( TZ43    .NE.  PZ41))                   STOP 56

  IF (ANY( T1%C    .NE.  (/(CHAR(I), I=1,6)/)))    STOP 62
  IF (ANY( T2%C    .NE.  (/(CHAR(I), I=1,6,2)/)))  STOP 64
  IF (ANY( T3%C    .NE.  P%C))                     STOP 66


  END



