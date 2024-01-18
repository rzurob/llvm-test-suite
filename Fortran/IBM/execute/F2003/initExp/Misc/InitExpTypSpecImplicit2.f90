!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecImplicit2.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 30, 2006
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
!*  intrinsic-type-spec in implicit stmt 
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecImplicit2 
  IMPLICIT  CHARACTER(KIND=KIND(0_1))           (A)
  IMPLICIT  CHARACTER(LEN=A%LEN+1, KIND=A%KIND) (B)
  IMPLICIT  CHARACTER(B%LEN+1,     KIND=B%KIND) (C)

  INTEGER :: I
  
  DIMENSION    :: A(128)
  PARAMETER      (A=[(CHAR(I), I=0, 127)])
  DIMENSION    :: B(128)
  PARAMETER      (B=[(CHAR(I), I=0, 127)])
  DIMENSION    :: C(128)
  PARAMETER      (C=[(CHAR(I), I=0, 127)])

  IF ( KIND(A) .NE. 1  )                       STOP 11
  IF (  LEN(A) .NE. 1  )                       STOP 12
  IF (  ANY(A  .NE. (/(CHAR(I), I=0, 127)/)) ) STOP 13

  IF ( KIND(B) .NE. 1  )                       STOP 21
  IF (  LEN(B) .NE. 2  )                       STOP 22
  IF (  ANY(B  .NE. (/(CHAR(I)//" ", I=0, 127)/)) ) STOP 23

  IF ( KIND(C) .NE. 1  )                       STOP 31
  IF (  LEN(C) .NE. 3  )                       STOP 32
  IF (  ANY(C  .NE. (/(CHAR(I)//"  ", I=0, 127)/)) ) STOP 33

  END

 
