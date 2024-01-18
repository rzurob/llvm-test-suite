!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemIBITS.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 10, 2006
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
!*  a reference to an elemental intrinsic
!* 
!*  - IBITS 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIBITS
  IMPLICIT NONE 

  INTEGER :: I, J

  INTEGER(1),                                       PARAMETER :: I1=-1
  INTEGER(1),                                       PARAMETER :: I1POS=3
  INTEGER(1),                                       PARAMETER :: I1LEN=3
  INTEGER(KIND(IBITS(POS=I1POS, LEN=I1LEN, I=I1))), PARAMETER :: & 
           TI1=IBITS(POS=I1POS, LEN=I1LEN, I=I1)

  INTEGER(2),                                       PARAMETER :: I2=-1
  INTEGER(2),                                       PARAMETER :: I2POS(128)=3
  INTEGER(2),                                       PARAMETER :: I2LEN=3
  INTEGER(KIND(IBITS(POS=I2POS, LEN=I2LEN, I=I2))), PARAMETER :: & 
      TI2(128)=IBITS(POS=I2POS, LEN=I2LEN, I=I2)

  INTEGER(4),                                       PARAMETER :: I4(128)=-1
  INTEGER(1),                                       PARAMETER :: I4POS=3
  INTEGER(4),                                       PARAMETER :: I4LEN(128)=3
  INTEGER(KIND(IBITS(POS=I4POS, LEN=I4LEN, I=I4))), PARAMETER :: & 
      TI4(128)=IBITS(POS=I4POS, LEN=I4LEN, I=I4)

  INTEGER(8),                                       PARAMETER :: I8(128)=-1
  INTEGER(4),                                       PARAMETER :: I8POS(128)=3
  INTEGER(1),                                       PARAMETER :: I8LEN(128)=3
  INTEGER(KIND(IBITS(POS=I8POS, LEN=I8LEN, I=I8))), PARAMETER :: & 
      TI8(128)=IBITS(POS=I8POS, LEN=I8LEN, I=I8)



   IF ( KIND(TI1)  .NE. 1 )         STOP 11
   IF (      TI1   .NE. 7 )         STOP 12
 
   IF ( KIND(TI2)  .NE. 2 )         STOP 21
   IF ( ANY (TI2   .NE. 7 ))        STOP 22
 
   IF ( KIND(TI4)  .NE. 4 )         STOP 31
   IF ( ANY (TI4   .NE. 7 ))        STOP 32
 
   IF ( KIND(TI8)  .NE. 8 )         STOP 41
   IF ( ANY (TI8   .NE. 7 ))        STOP 42
 
   DO I=3, 5 
     SELECT CASE (I)
     CASE (IBITS(POS=0, LEN=3, I=3):IBITS(POS=0, LEN=3, I=3))
       PRINT*, I 
     CASE (IBITS(POS=0, LEN=3, I=4):IBITS(POS=0, LEN=3, I=4))
       PRINT*, I 
     CASE (IBITS(POS=0, LEN=3, I=5):IBITS(POS=0, LEN=3, I=5))
       PRINT*, I 
     CASE DEFAULT
       PRINT*, I 
       STOP 44 
     END SELECT
   END DO




  END


