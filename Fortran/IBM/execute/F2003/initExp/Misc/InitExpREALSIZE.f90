!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpREALSIZE.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 25, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qrealsize 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  -qrealsize 
!* 
!*  (322381)          
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpREALSIZE
  IMPLICIT NONE

  INTEGER  :: I 
  REAL     :: R
  COMPLEX  :: Z

  REAL,                                           PARAMETER :: RArr1(128)=(/(1., I=0, 127)/)  
  REAL,                                           PARAMETER :: RArr2(128)=(/(2., I=0, 127)/)  
  REAL(KIND(REAL(RArr1, KIND=RArr2%KIND))),       PARAMETER :: RArr(128) =REAL(RArr1, KIND=RArr2%KIND)

  COMPLEX,                                        PARAMETER :: ZArr1(128)=(/((1.,1.), I=0, 127)/)  
  COMPLEX,                                        PARAMETER :: ZArr2(128)=(/((0.,0.), I=0, 127)/)  
  COMPLEX(KIND(CMPLX(ZArr1, KIND=ZArr2%KIND))),   PARAMETER :: ZArr(128) = CMPLX(ZArr1, KIND=ZArr2%KIND) 


  IF ( KIND(RArr) .NE. KIND(R))  STOP 11
  IF ( ANY( RArr  .NE. 1 ))      STOP 12

  IF ( KIND(ZArr) .NE. KIND(Z))  STOP 21
  IF ( ANY( ZArr  .NE. (1.,1.))) STOP 22


  END


