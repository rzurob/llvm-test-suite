!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefSPREAD.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 30, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  a reference to an tranformational intrinsic
!* 
!*  - SPREAD 
!*  (319474)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefSPREAD 
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1), PARAMETER :: S11    = 6 
  INTEGER(1), PARAMETER :: S12(6) = (/1,2,3,4,5,6/) 
  INTEGER(1), PARAMETER :: NC11   = 6 

  INTEGER(KIND(SPREAD(S11, 1, 0)))    :: TI11 = SIZE(SPREAD(S11, 1, 0)) 
  INTEGER(KIND(SPREAD(S11, 1, NC11))) :: TI12(SIZE(SPREAD(S11, 1, NC11))) = SPREAD(S11, 1, NC11) 
  INTEGER(KIND(SPREAD(S12, 1, 0)))    :: TI13(2)= SHAPE( SPREAD(S12, 1, 0)) 
  INTEGER(KIND(SPREAD(S12, 1, NC11))) :: TI14(SIZE(SPREAD(S12(1), 1, NC11)),6) = SPREAD(S12, 1, NC11) 

  LOGICAL(2), PARAMETER :: SL21    = .TRUE. 
  LOGICAL(2), PARAMETER :: SL22(6) = (/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./) 
  INTEGER(2), PARAMETER :: NCL21   = 6 

  INTEGER(KIND(SPREAD(SL21, 1, 0)))    :: TL21 = SIZE(SPREAD(SL21, 1, 0)) 
  LOGICAL(KIND(SPREAD(SL21, 1, NC11))) :: TL22(SIZE(SPREAD(SL21, 1, NCL21))) = SPREAD(SL21, 1, NCL21) 
  INTEGER(KIND(SPREAD(SL22, 1, 0)))    :: TL23(2)= SHAPE( SPREAD(SL22, 1, 0)) 
  LOGICAL(KIND(SPREAD(SL22, 1, NC11))) :: TL24(SIZE(SPREAD(SL22(1), 1, NCL21)),6) = SPREAD(SL22, 1, NCL21) 

  REAL(4), PARAMETER :: SR41    = 6 
  REAL(4), PARAMETER :: SR42(6) = (/1,2,3,4,5,6/) 
  INTEGER(4), PARAMETER :: NCR41   = 6 

  REAL(KIND(SPREAD(SR41, 1, 0)))    :: TR41 = SIZE(SPREAD(SR41, 1, 0)) 
  REAL(KIND(SPREAD(SR41, 1, NC11))) :: TR42(SIZE(SPREAD(SR41, 1, NCR41))) = SPREAD(SR41, 1, NCR41) 
  REAL(KIND(SPREAD(SR42, 1, 0)))    :: TR43(2)= SHAPE( SPREAD(SR42, 1, 0)) 
  REAL(KIND(SPREAD(SR42, 1, NC11))) :: TR44(SIZE(SPREAD(SR42(6), 1, NCR41)),6) = SPREAD(SR42, 1, NCR41) 

  COMPLEX(8), PARAMETER :: SZ81    = (6,6) 
  COMPLEX(8), PARAMETER :: SZ82(6) = (/(1,1),(2,2),(3,3),(4,4),(5,5),(6,6)/) 
  INTEGER(8), PARAMETER :: NCZ81   = 6 

  INTEGER(KIND(SPREAD(SZ81, 1, 0)))    :: TZ81 = SIZE(SPREAD(SZ81, 1, 0)) 
  COMPLEX(KIND(SPREAD(SZ81, 1, NC11))) :: TZ82(SIZE(SPREAD(SZ81, 1, NCZ81))) = SPREAD(SZ81, 1, NCZ81) 
  INTEGER(KIND(SPREAD(SZ82, 1, 0)))    :: TZ83(2)= SHAPE( SPREAD(SZ82, 1, 0)) 
  COMPLEX(KIND(SPREAD(SZ82, 1, NC11))) :: TZ84(SIZE(SPREAD(SZ82(1), 1, NCZ81)),6) = SPREAD(SZ82, 1, NCZ81) 

 
  IF(  KIND(TI11)     .NE. 1 )      STOP 11
  IF(  TI11           .NE. 0 )      STOP 12
  IF(  KIND(TI12)     .NE. 1 )      STOP 13
  IF(  SIZE(TI12)     .NE. NC11 )   STOP 14
  IF(  ANY(TI12       .NE. 6 ))     STOP 15
  IF(  KIND(TI13)     .NE. 1 )      STOP 16
  IF(  ANY(TI13       .NE. (/0,6/))) STOP 17
  IF(  KIND(TI14)     .NE. 1 )      STOP 18
  IF(  SIZE(TI14,1) .NE. NC11 )   STOP 19
  DO I=1,NC11
    IF(  ANY(TI14(I,:)  .NE. S12 )) STOP 10
  END DO

  IF(  KIND(TL21)     .NE. 2 )           STOP 21
  IF(  TL21           .NE. 0 )           STOP 22
  IF(  KIND(TL22)     .NE. 2 )           STOP 23
  IF(  SIZE(TL22)     .NE. NCL21 )       STOP 24
  IF(  ANY(TL22       .NEQV. .TRUE. ))   STOP 25
  IF(  KIND(TL23)     .NE. 2 )           STOP 26
  IF(  ANY(TL23       .NE. (/0,6/) ))    STOP 27
  IF(  KIND(TL24)     .NE. 2 )           STOP 28
  IF(  SIZE(TL24,1)   .NE. NCL21 )       STOP 29
  DO I=1,NC11
    IF(  ANY(TL24(I,:)  .NEQV. SL22 ))   STOP 20
  END DO

  IF(  KIND(TR41)     .NE. 4 )      STOP 31
  IF(  TR41           .NE. 0 )      STOP 32
  IF(  KIND(TR42)     .NE. 4 )      STOP 33
  IF(  SIZE(TR42)     .NE. NCR41 )  STOP 34
  IF(  ANY(TR42       .NE. 6 ))     STOP 35
  IF(  KIND(TR43)     .NE. 4 )      STOP 36
  IF(  ANY(TR43       .NE. (/0,6/)))STOP 37
  IF(  KIND(TR44)     .NE. 4 )      STOP 38
  IF(  SIZE(TR44,1)   .NE. NCR41 )  STOP 39
  DO I=1,NC11
    IF(  ANY(TR44(I,:)  .NE. SR42 ))STOP 30
  END DO

  IF(  KIND(TZ81)     .NE. 8 )      STOP 41
  IF(  TZ81           .NE. 0 )      STOP 42
  IF(  KIND(TZ82)     .NE. 8 )      STOP 43
  IF(  SIZE(TZ82)     .NE. 6 )      STOP 44
  IF(  ANY(TZ82       .NE. SZ81 ))  STOP 45
  IF(  KIND(TZ83)     .NE. 8 )      STOP 46
  IF(  ANY(TZ83       .NE. (/0,6/)))STOP 47
  IF(  KIND(TZ84)     .NE. 8 )      STOP 48
  IF(  SIZE(TZ84,1) .NE. NCZ81 )    STOP 49
  DO I=1,NC11
    IF(  ANY(TZ84(I,:)  .NE. SZ82 ))STOP 40
  END DO


  END


 
