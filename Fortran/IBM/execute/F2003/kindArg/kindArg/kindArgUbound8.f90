!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgUbound8
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 05, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : UBOUND 
!*
!*  REFERENCE                  : Feature Number 289083 
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
!*   
!*  -- assumed-size array, Dim < n, n is the rank of the array.
!* 
!*  (322407) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgUbound8
  IMPLICIT NONE

  INTEGER :: I, I1
  
  TYPE :: DT
    CHARACTER :: C
    LOGICAL(2):: L
    INTEGER   :: I
    REAL(8)   :: R
    COMPLEX   :: Z
  END TYPE

  TYPE :: DT1
    CLASS(*), POINTER :: Arr(:,:,:,:,:,:,:,:,:)
    CLASS(*), POINTER :: Arr0(:,:,:,:,:,:,:,:,:)
  END TYPE

  TYPE(DT1)          :: TT
  TYPE(DT )          :: T 
  INTEGER, PARAMETER :: L1=-1
  INTEGER, PARAMETER :: L2=-0
  INTEGER, PARAMETER :: S=2**9 
   

  CALL IntSub((/(DT1(NULL(),NULL()), I=1, 2**9)/), (/DT1::/))

  CONTAINS

  SUBROUTINE IntSub(Arr, Arr0)
  TYPE (DT1) :: Arr(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:*)
  TYPE (DT1) :: Arr0(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:*)

  DO I = 1, 8
    IF (     UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%C))   .NE. L2)               STOP 11
    IF (KIND(UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%C)))  .NE. 1)                STOP 12
    IF (     UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%L))   .NE. L2)               STOP 13
    IF (KIND(UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%L)))  .NE. 2)                STOP 14
    IF (     UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%I))   .NE. L2)               STOP 15
    IF (KIND(UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%I)))  .NE. 4)                STOP 16
    IF (     UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%R))   .NE. L2)               STOP 17
    IF (KIND(UBOUND(ARRAY=Arr0, DIM=I, KIND=KIND(T%R)))  .NE. 8)                STOP 18
  END DO



  DO I = 1, 8
    IF (     UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%C))   .NE. L2)               STOP 61
    IF (KIND(UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%C)))  .NE. 1)                STOP 62
    IF (     UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%L))   .NE. L2)               STOP 63
    IF (KIND(UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%L)))  .NE. 2)                STOP 64
    IF (     UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%I))   .NE. L2)               STOP 65
    IF (KIND(UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%I)))  .NE. 4)                STOP 66
    IF (     UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%R))   .NE. L2)               STOP 67
    IF (KIND(UBOUND(ARRAY=Arr, DIM=I, KIND=KIND(T%R)))  .NE. 8)                STOP 68
  END DO


  END SUBROUTINE


  END

