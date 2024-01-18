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
!*  - UNPACK
!*  (319511)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER    :: I
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

  PROGRAM   InitExpDefUNPACK
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  LOGICAL,      PARAMETER :: T=.TRUE., F=.FALSE.

  INTEGER(1),   PARAMETER :: I11(3)    = (/(I, I=1,3)/)
  LOGICAL(1),   PARAMETER :: MI11(3,3) = RESHAPE((/F,T,F,T,F,F,F,F,T/), (/3,3/))
  INTEGER(1),   PARAMETER :: FI11(3,3) = RESHAPE((/1,0,0,0,1,0,0,0,1/), (/3,3/))

  INTEGER(KIND(UNPACK(I11, MASK=MI11, FIELD=FI11))),   PARAMETER ::   &
                           TI11(3,3)=UNPACK(I11, MASK=MI11, FIELD=FI11)
  INTEGER(KIND(UNPACK(I11, MASK=MI11, FIELD=0_1))),      PARAMETER ::   &
                           TI12(3,3)=UNPACK(I11, MASK=MI11, FIELD=0_1)
  INTEGER,      PARAMETER :: IR1(3,3) = RESHAPE((/1,1,0,2,1,0,0,0,3/), (/3,3/))
  INTEGER,      PARAMETER :: IR2(3,3) = RESHAPE((/0,1,0,2,0,0,0,0,3/), (/3,3/))

  LOGICAL(2),   PARAMETER :: L21(3)    = (/T,T,T/)
  LOGICAL(2),   PARAMETER :: ML21(3,3) = RESHAPE((/F,T,F,T,F,F,F,F,T/), (/3,3/))
  LOGICAL(2),   PARAMETER :: FL21(3,3) = RESHAPE((/T,F,F,F,T,F,F,F,T/), (/3,3/))

  LOGICAL(KIND(UNPACK(L21, MASK=ML21, FIELD=FL21))),   PARAMETER ::   &
                           TL21(3,3)=UNPACK(L21, MASK=ML21, FIELD=FL21)
  LOGICAL(KIND(UNPACK(L21, MASK=ML21, FIELD=.FALSE._2))),      PARAMETER ::   &
                           TL22(3,3)=UNPACK(L21, MASK=ML21, FIELD=.FALSE._2)
  LOGICAL,      PARAMETER :: LR1(3,3) = RESHAPE((/T,T,F,T,T,F,F,F,T/), (/3,3/))
  LOGICAL,      PARAMETER :: LR2(3,3) = RESHAPE((/F,T,F,T,F,F,F,F,T/), (/3,3/))

  REAL(4),      PARAMETER :: R41(3)    = (/(I, I=1,3)/)
  LOGICAL(4),   PARAMETER :: MR41(3,3) = RESHAPE((/F,T,F,T,F,F,F,F,T/), (/3,3/))
  REAL(4),      PARAMETER :: FR41(3,3) = RESHAPE((/1,0,0,0,1,0,0,0,1/), (/3,3/))

  REAL(KIND(UNPACK(R41, MASK=MR41, FIELD=FR41))),   PARAMETER ::   &
                           TR41(3,3)=UNPACK(R41, MASK=MR41, FIELD=FR41)
  REAL(KIND(UNPACK(R41, MASK=MR41, FIELD=0._4))),      PARAMETER ::   &
                           TR42(3,3)=UNPACK(R41, MASK=MR41, FIELD=0._4)
  REAL,      PARAMETER :: RR1(3,3) = RESHAPE((/1,1,0,2,1,0,0,0,3/), (/3,3/))
  REAL,      PARAMETER :: RR2(3,3) = RESHAPE((/0,1,0,2,0,0,0,0,3/), (/3,3/))

  COMPLEX(8),      PARAMETER :: Z81(3)    = (/((I,I), I=1,3)/)
  LOGICAL(4),      PARAMETER :: MZ81(3,3) = RESHAPE((/F,T,F,T,F,F,F,F,T/), (/3,3/))
  COMPLEX(8),      PARAMETER :: FZ81(3,3) =    &
                   RESHAPE((/(1,1),(0,0),(0,0),(0,0),(1,1),(0,0),(0,0),(0,0),(1,1)/), (/3,3/))

  COMPLEX(KIND(UNPACK(Z81, MASK=MZ81, FIELD=FZ81))),   PARAMETER ::   &
                           TZ81(3,3)=UNPACK(Z81, MASK=MZ81, FIELD=FZ81)
  COMPLEX(KIND(UNPACK(Z81, MASK=MZ81, FIELD=(0._8,0._8)))),      PARAMETER ::   &
                           TZ82(3,3)=UNPACK(Z81, MASK=MZ81, FIELD=(0._8,0._8))
  COMPLEX,      PARAMETER :: ZR1(3,3) =  &
                RESHAPE((/(1,1),(1,1),(0,0),(2,2),(1,1),(0,0),(0,0),(0,0),(3,3)/), (/3,3/))
  COMPLEX,      PARAMETER :: ZR2(3,3) =  &
                RESHAPE((/(0,0),(1,1),(0,0),(2,2),(0,0),(0,0),(0,0),(0,0),(3,3)/), (/3,3/))

  TYPE(DT),     PARAMETER :: D(3)    = (/(DT(I=I), I=1,3)/)
  LOGICAL(1),   PARAMETER :: MD(3,3) = RESHAPE((/F,T,F,T,F,F,F,F,T/), (/3,3/))
  TYPE(DT),     PARAMETER :: FD(3,3) =     &
                RESHAPE((/DT(I=1),DT(I=0),DT(I=0),DT(I=0),DT(I=1),DT(I=0),DT(I=0),DT(I=0),DT(I=1)/), (/3,3/))

  TYPE(DT),     PARAMETER ::  TD1(3,3)=UNPACK(D, MASK=MD, FIELD=FD)
  TYPE(DT),     PARAMETER ::  TD2(3,3)=UNPACK(D, MASK=MD, FIELD=DT(I=0))
  TYPE(DT),     PARAMETER ::  DR1(3,3) =   &
                RESHAPE((/DT(I=1),DT(I=1),DT(I=0),DT(I=2),DT(I=1),DT(I=0),DT(I=0),DT(I=0),DT(I=3)/), (/3,3/))
  TYPE(DT),     PARAMETER ::  DR2(3,3) =   &
                RESHAPE((/DT(I=0),DT(I=1),DT(I=0),DT(I=2),DT(I=0),DT(I=0),DT(I=0),DT(I=0),DT(I=3)/), (/3,3/))


  IF (KIND(TI11)   .NE.   1 )        STOP 11
  IF (ANY( TI11    .NE.   IR1))      STOP 12
  IF (KIND(TI12)   .NE.   1 )        STOP 13
  IF (ANY( TI12    .NE.   IR2))      STOP 14

  IF (KIND(TL21)   .NE.   2 )        STOP 21
  IF (ANY( TL21    .NEQV. LR1))      STOP 22
  IF (KIND(TL22)   .NE.   2 )        STOP 23
  IF (ANY( TL22    .NEQV. LR2))      STOP 24

  IF (KIND(TR41)   .NE.   4 )        STOP 31
  IF (ANY( TR41    .NE.   RR1))      STOP 32
  IF (KIND(TR42)   .NE.   4 )        STOP 33
  IF (ANY( TR42    .NE.   RR2))      STOP 34

  IF (KIND(TZ81)   .NE.   8 )        STOP 41
  IF (ANY( TZ81    .NE.   ZR1))      STOP 42
  IF (KIND(TZ82)   .NE.   8 )        STOP 43
  IF (ANY( TZ82    .NE.   ZR2))      STOP 44

  IF (ANY( TD1%I   .NE.   DR1%I))    STOP 52
  IF (ANY( TD2%I   .NE.   DR2%I))    STOP 54



  END



