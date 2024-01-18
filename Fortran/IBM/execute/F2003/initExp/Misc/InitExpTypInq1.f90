!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypInq1.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 22, 2006
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
!*  Type Parameter Inquiry on intrinsic types 
!* 
!* (324775/324382) 
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypInq1 
  IMPLICIT NONE

  INTEGER(1), ALLOCATABLE :: I1(:)
  INTEGER(2), ALLOCATABLE :: I2(:)
  INTEGER(4), ALLOCATABLE :: I4(:)
  INTEGER(8), ALLOCATABLE :: I8(:)

  LOGICAL(1), ALLOCATABLE :: L1(:)
  LOGICAL(2), ALLOCATABLE :: L2(:)
  LOGICAL(4), ALLOCATABLE :: L4(:)
  LOGICAL(8), ALLOCATABLE :: L8(:)

  REAL(4), POINTER  :: R4(:)
  REAL(8), POINTER  :: R8(:)
  REAL(16),POINTER  :: R6(:)

  COMPLEX(4), ALLOCATABLE  :: Z4(:)
  COMPLEX(8), ALLOCATABLE  :: Z8(:)
  COMPLEX(16),ALLOCATABLE  :: Z6(:)

  CHARACTER(128), POINTER :: C(:)

  TYPE :: DT
    INTEGER(1) :: I1=I1%KIND
    INTEGER(2) :: I2=I2%KIND
    INTEGER(4) :: I4=I4%KIND
    INTEGER(8) :: I8=I8%KIND

    INTEGER(1) :: L1=L1%KIND
    INTEGER(2) :: L2=L2%KIND
    INTEGER(4) :: L4=L4%KIND
    INTEGER(8) :: L8=L8%KIND

    INTEGER(4) :: R4=R4%KIND
    INTEGER(8) :: R8=R8%KIND
    INTEGER(1) :: R6=R6%KIND

    INTEGER(4) :: Z4=Z4%KIND
    INTEGER(8) :: Z8=Z8%KIND
    INTEGER(1) :: Z6=Z6%KIND

    CHARACTER(C%LEN) :: CC=ACHAR(C%KIND)
  END TYPE
 
  TYPE(DT), PARAMETER ::  T=DT()
 
 
  IF (T%I1    .NE. 1  ) STOP 11
  IF (T%I2    .NE. 2  ) STOP 12
  IF (T%I4    .NE. 4  ) STOP 14
  IF (T%I8    .NE. 8  ) STOP 18

  IF (T%L1    .NE. 1  ) STOP 21
  IF (T%L2    .NE. 2  ) STOP 22
  IF (T%L4    .NE. 4  ) STOP 24
  IF (T%L8    .NE. 8  ) STOP 28

  IF (T%R4    .NE. 4  ) STOP 31
  IF (T%R8    .NE. 8  ) STOP 32
  IF (T%R6    .NE. 16 ) STOP 33

  IF (T%Z4    .NE. 4  ) STOP 41
  IF (T%Z8    .NE. 8  ) STOP 42
  IF (T%Z6    .NE. 16 ) STOP 43

  IF (T%CC    .NE. ACHAR(1) ) STOP 51
  IF (LEN(T%CC).NE. 128)      STOP 52


  END

 
