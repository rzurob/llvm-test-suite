!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefSUM.f  
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
!*  - SUM
!*  (319478)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefSUM 
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1), PARAMETER :: I11(3,3)   = 1 
  INTEGER(1), PARAMETER :: I12(3,3:2) = 1 
  LOGICAL(1), PARAMETER :: MI11(3,3)  = RESHAPE((/(/(.TRUE., I=1,3)/),(/(.FALSE., I=1,7)/)/), (/3,3/)) 
  LOGICAL(1), PARAMETER :: MI12(3,3)  = .FALSE. 
 
  INTEGER(KIND(SUM(I11))),        PARAMETER :: TI11 = SUM(I11)
  INTEGER(KIND(SUM(I12))),        PARAMETER :: TI12 = SUM(I12)
  INTEGER(KIND(SUM(I11,MI11))),   PARAMETER :: TI13 = SUM(I11,MI11) 
  INTEGER(KIND(SUM(I11,MI12))),   PARAMETER :: TI14 = SUM(I11,MI12) 
  INTEGER(KIND(SUM(I11,1,MI11))), PARAMETER :: TI15(SIZE(SUM(I11,DIM=1,MASK=MI11))) = SUM(I11,DIM=1,MASK=MI11) 

  REAL(8), PARAMETER :: R81(3,3)    = 1 
  REAL(8), PARAMETER :: R82(3,3:2)  = 1 
  LOGICAL(8), PARAMETER :: MR81(3,3)  = RESHAPE((/(/(.TRUE., I=1,3)/),(/(.FALSE., I=1,7)/)/), (/3,3/)) 
  LOGICAL(8), PARAMETER :: MR82(3,3)  = .FALSE. 
 
  REAL(KIND(SUM(R81))),        PARAMETER :: TR81 = SUM(R81)
  REAL(KIND(SUM(R82))),        PARAMETER :: TR82 = SUM(R82)
  REAL(KIND(SUM(R81,MR81))),   PARAMETER :: TR83 = SUM(R81,MR81) 
  REAL(KIND(SUM(R81,MR82))),   PARAMETER :: TR84 = SUM(R81,MR82) 
  REAL(KIND(SUM(R81,1,MR81))), PARAMETER :: TR85(SIZE(SUM(R81,DIM=1,MASK=MR81))) = SUM(R81,DIM=1,MASK=MR81) 

  COMPLEX(8), PARAMETER :: Z81(3,3)    = (1,1) 
  COMPLEX(8), PARAMETER :: Z82(3,3:2)  = (1,1) 
  LOGICAL(8), PARAMETER :: MZ81(3,3)  = RESHAPE((/(/(.TRUE., I=1,3)/),(/(.FALSE., I=1,7)/)/), (/3,3/)) 
  LOGICAL(8), PARAMETER :: MZ82(3,3)  = .FALSE. 
 
  COMPLEX(KIND(SUM(Z81))),        PARAMETER :: TZ81 = SUM(z81)
  COMPLEX(KIND(SUM(Z82))),        PARAMETER :: TZ82 = SUM(Z82)
  COMPLEX(KIND(SUM(Z81,MZ81))),   PARAMETER :: TZ83 = SUM(Z81,MZ81) 
  COMPLEX(KIND(SUM(Z81,MZ82))),   PARAMETER :: TZ84 = SUM(Z81,MZ82) 
  COMPLEX(KIND(SUM(Z81,1,MZ81))), PARAMETER :: TZ85(SIZE( SUM(Z81,DIM=1,MASK=MZ81))) = SUM(Z81,DIM=1,MASK=MZ81) 

 
  IF(  KIND(TI11)     .NE. 1 )     STOP 10
  IF(  TI11           .NE. 9 )     STOP 11
  IF(  KIND(TI12)     .NE. 1 )     STOP 12
  IF(  TI12           .NE. 0 )     STOP 13
  IF(  KIND(TI13)     .NE. 1 )     STOP 14
  IF(  TI13           .NE. 3 )     STOP 15
  IF(  KIND(TI14)     .NE. 1 )     STOP 16
  IF(  TI14           .NE. 0 )     STOP 17
  IF(  KIND(TI15)     .NE. 1 )     STOP 18
  IF(  ANY(TI15       .NE. (/3,0,0/))) STOP 19


  IF(  KIND(TR81)     .NE. 8 )     STOP 20
  IF(  TR81           .NE. 9 )     STOP 21
  IF(  KIND(TR82)     .NE. 8 )     STOP 22
  IF(  TR82           .NE. 0 )     STOP 23
  IF(  KIND(TR83)     .NE. 8 )     STOP 24
  IF(  TR83           .NE. 3 )     STOP 25
  IF(  KIND(TR84)     .NE. 8 )     STOP 26
  IF(  TR84           .NE. 0 )     STOP 27
  IF(  KIND(TR85)     .NE. 8 )     STOP 28
  IF(  ANY(TR85       .NE. (/3,0,0/))) STOP 29


  IF(  KIND(TZ81)     .NE. 8 )     STOP 30
  IF(  TZ81           .NE. (9,9) ) STOP 31
  IF(  KIND(TZ82)     .NE. 8 )     STOP 32
  IF(  TZ82           .NE. 0 )     STOP 33
  IF(  KIND(TZ83)     .NE. 8 )     STOP 34
  IF(  TZ83           .NE. (3,3) ) STOP 35
  IF(  KIND(TZ84)     .NE. 8 )     STOP 36
  IF(  TZ84           .NE. 0 )     STOP 37
  IF(  KIND(TZ85)     .NE. 8 )     STOP 38
  IF(  ANY(TZ85       .NE. (/(3,3),(0,0),(0,0)/))) STOP 39


  END


 
