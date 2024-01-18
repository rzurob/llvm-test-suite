! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/F2003/kindArg/kindArg/kindArgSize5.f
! opt variations: -qnok -qnol -qdeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgSize5
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 30, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : SIZE 
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
!*  Result Characteristics.
!*  Integer scalar. If KIND is present, the kind type parameter is that specified by the value of KIND; 
!*  otherwise the kind type parameter is that of default integer type.
!*
!*    
!*  (322836) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgSize5


  INTEGER(1), ALLOCATABLE ::  K1
  INTEGER(2), POINTER     ::  K2
  INTEGER(4), ALLOCATABLE ::  K4
  INTEGER(8), POINTER     ::  K8
    
  INTEGER :: I
 
  TYPE :: DT(D1,N1)    ! (4,20)
      INTEGER, KIND :: D1
      INTEGER, LEN  :: N1
  END TYPE

  CLASS(DT(4,20)), POINTER    :: CC(:,:,:,:,:,:,:,:,:)


  ALLOCATE( CC(1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9))


  IF (     SIZE(CC )                                 .NE. 1)              STOP 11
  IF (KIND(SIZE(CC ))                                .NE. 4)              STOP 12

  IF (     SIZE(CC,   KIND=K1%KIND )                 .NE. 1)              STOP 13
  IF (KIND(SIZE(CC,   KIND=K1%KIND ))                .NE. K1%KIND)        STOP 14
  DO I = 1, 9 
    IF (     SIZE(CC, KIND=K1%KIND, DIM=I )          .NE. 1)              STOP 15
    IF (KIND(SIZE(CC, KIND=K1%KIND, DIM=I ))         .NE. K1%KIND)        STOP 16
  END DO


  IF (     SIZE(CC,   KIND=K2%KIND )                 .NE. 1)              STOP 23
  IF (KIND(SIZE(CC,   KIND=K2%KIND ))                .NE. K2%KIND)        STOP 24
  DO I = 1, 9 
    IF (     SIZE(CC, KIND=K2%KIND, DIM=I )          .NE. 1)              STOP 25
    IF (KIND(SIZE(CC, KIND=K2%KIND, DIM=I ))         .NE. K2%KIND)        STOP 26
  END DO


  IF (     SIZE(CC,   KIND=K4%KIND )                 .NE. 1)              STOP 43
  IF (KIND(SIZE(CC,   KIND=K4%KIND ))                .NE. K4%KIND)        STOP 44
  DO I = 1, 9
    IF (     SIZE(CC, KIND=K4%KIND, DIM=I )          .NE. 1)              STOP 45
    IF (KIND(SIZE(CC, KIND=K4%KIND, DIM=I ))         .NE. K4%KIND)        STOP 46
  END DO


  IF (     SIZE(CC,   KIND=K8%KIND )                 .NE. 1)              STOP 53
  IF (KIND(SIZE(CC,   KIND=K8%KIND ))                .NE. K8%KIND)        STOP 54
  DO I = 1, 9 
    IF (     SIZE(CC, KIND=K8%KIND, DIM=I )          .NE. 1)              STOP 55
    IF (KIND(SIZE(CC, KIND=K8%KIND, DIM=I ))         .NE. K8%KIND)        STOP 56
  END DO


  DEALLOCATE( CC )
  ALLOCATE( CC(1:0,2:1,3:2,4:3,5:4,6:5,7:6,8:7,9:8))


  IF (     SIZE(CC )                                 .NE. 0)              STOP 61
  IF (KIND(SIZE(CC ))                                .NE. 4)              STOP 62

  IF (     SIZE(CC,   KIND=K1%KIND )                 .NE. 0)              STOP 63
  IF (KIND(SIZE(CC,   KIND=K1%KIND ))                .NE. K1%KIND)        STOP 64
  DO I = 1, 9 
    IF (     SIZE(CC, KIND=K1%KIND, DIM=I )          .NE. 0)              STOP 65
    IF (KIND(SIZE(CC, KIND=K1%KIND, DIM=I ))         .NE. K1%KIND)        STOP 66
  END DO


  IF (     SIZE(CC,   KIND=K2%KIND )                 .NE. 0)              STOP 73
  IF (KIND(SIZE(CC,   KIND=K2%KIND ))                .NE. K2%KIND)        STOP 74
  DO I = 1, 9 
    IF (     SIZE(CC, KIND=K2%KIND, DIM=I )          .NE. 0)              STOP 75
    IF (KIND(SIZE(CC, KIND=K2%KIND, DIM=I ))         .NE. K2%KIND)        STOP 76
  END DO


  IF (     SIZE(CC,   KIND=K4%KIND )                 .NE. 0)              STOP 83
  IF (KIND(SIZE(CC,   KIND=K4%KIND ))                .NE. K4%KIND)        STOP 84
  DO I = 1, 9
    IF (     SIZE(CC, KIND=K4%KIND, DIM=I )          .NE. 0)              STOP 85
    IF (KIND(SIZE(CC, KIND=K4%KIND, DIM=I ))         .NE. K4%KIND)        STOP 86
  END DO


  IF (     SIZE(CC,   KIND=K8%KIND )                 .NE. 0)              STOP 93
  IF (KIND(SIZE(CC,   KIND=K8%KIND ))                .NE. K8%KIND)        STOP 94
  DO I = 1, 9 
    IF (     SIZE(CC, KIND=K8%KIND, DIM=I )          .NE. 0)              STOP 95
    IF (KIND(SIZE(CC, KIND=K8%KIND, DIM=I ))         .NE. K8%KIND)        STOP 96
  END DO




  END


