!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecAllocate.f  
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
!*  Type Spec in ALLOCATE STMT 
!* 
!* 
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM InitExpTypSpecAllocate 

  IMPLICIT INTEGER(KIND=1_8) (A)
  IMPLICIT INTEGER(KIND=2_4) (B)
  IMPLICIT INTEGER(KIND=4_2) (C)
  IMPLICIT INTEGER(KIND=8_1) (D)

  IMPLICIT LOGICAL(KIND=KIND((/0_1/))) (E)
  IMPLICIT LOGICAL(KIND=KIND((/0_2/))) (F)
  IMPLICIT LOGICAL(KIND=KIND((/0_4/))) (G)
  IMPLICIT LOGICAL(KIND=KIND((/0_8/))) (H)

  INTEGER, PARAMETER :: Res(128)=(/(-I, I=1, 128)/)
  LOGICAL, PARAMETER :: LRes1(128)=(/(.TRUE., I=1, 128)/)
  LOGICAL, PARAMETER :: LRes2(128)=(/(.FALSE., I=1, 128)/)

  ALLOCATABLE A(:)
  ALLOCATABLE B(:)
  ALLOCATABLE C(:)
  ALLOCATABLE D(:)

  ALLOCATABLE E(:)
  ALLOCATABLE F(:)
  ALLOCATABLE G(:)
  ALLOCATABLE H(:)

  ALLOCATE(INTEGER(KIND=A%KIND) :: A(128))
  ALLOCATE(INTEGER(KIND=B%KIND) :: B(128))
  ALLOCATE(INTEGER(KIND=C%KIND) :: C(128))
  ALLOCATE(INTEGER(KIND=D%KIND) :: D(128))

  ALLOCATE(LOGICAL(KIND=E%KIND) :: E(128))
  ALLOCATE(LOGICAL(KIND=F%KIND) :: F(128))
  ALLOCATE(LOGICAL(KIND=G%KIND) :: G(128))
  ALLOCATE(LOGICAL(KIND=H%KIND) :: H(128))

  IF ( KIND(A) .NE. 1     ) STOP 11
  IF ( KIND(B) .NE. 2     ) STOP 13
  IF ( KIND(C) .NE. 4     ) STOP 15
  IF ( KIND(D) .NE. 8     ) STOP 17

  IF ( KIND(E) .NE. 1       )   STOP 21
  IF ( KIND(F) .NE. 2       )   STOP 23
  IF ( KIND(G) .NE. 4       )   STOP 25
  IF ( KIND(H) .NE. 8       )   STOP 27

  END

 
