!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemNINT.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 12, 2006
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
!*  -  NINT 
!*  (318902/318985/319559)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemNINT
  IMPLICIT NONE 
  INTEGER :: I, J

  TYPE :: DT
    REAL(4)  :: R4=2.4 
    REAL(8)  :: R8=2.5 
    REAL(16) :: R6=2.6 
  END TYPE

  TYPE (DT), PARAMETER :: X(128) = DT()

  INTEGER(KIND( NINT(A=X%R4, KIND=1)))            :: TI1(128) = NINT(A=X%R4, KIND=1)
  INTEGER(KIND( NINT(A=X%R8, KIND=KIND(X%R8)/4))) :: TI2(128) = NINT(A=X%R8, KIND=KIND(X%R8)/4)
  INTEGER(KIND( NINT(A=X%R4, KIND=KIND(X%R4))))   :: TI4(128) = NINT(A=X%R4, KIND=KIND(X%R4))
  INTEGER(KIND( NINT(A=X%R6, KIND=KIND(X%R6)/2))) :: TI8(128) = NINT(A=X%R6, KIND=KIND(X%R6)/2)

  INTEGER :: T1=MAXVAL(NINT(A=X%R4, KIND=1))
  INTEGER :: T2=MAXVAL(NINT(A=X%R8, KIND=KIND(X%R8)/4))
  INTEGER :: T3=MAXVAL(NINT(A=X%R4, KIND=KIND(X%R4)))
  INTEGER :: T4=MAXVAL(NINT(A=X%R6, KIND=KIND(X%R6)/2))

  IF ( KIND(TI1)  .NE.    1 )  STOP 11
  IF ( KIND(TI2)  .NE.    2 )  STOP 12
  IF ( KIND(TI4)  .NE.    4 )  STOP 14
  IF ( KIND(TI8)  .NE.    8 )  STOP 18

  IF (ANY( TI1  .NE. 2 )) STOP 21
  IF (ANY( TI2  .NE. 3 )) STOP 22
  IF (ANY( TI4  .NE. 2 )) STOP 23
  IF (ANY( TI8  .NE. 3 )) STOP 24

  IF ( T1   .NE. 2 )  STOP 31
  IF ( T2   .NE. 3 )  STOP 32
  IF ( T3   .NE. 2 )  STOP 33
  IF ( T4   .NE. 3 )  STOP 34


  END


