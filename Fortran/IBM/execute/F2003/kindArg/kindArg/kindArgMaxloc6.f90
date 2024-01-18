!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMaxloc6
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 27, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : MAXLOC 
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
!*  Result Value.
!*  The result of MAXLOC (ARRAY) is a rank-one array whose element values are
!*  the values of the subscripts of an element of ARRAY whose value equals the
!*  maximum value of all of the elements of ARRAY. The ith subscript returned lies
!*  in the range 1 to ei, where ei is the extent of the ith dimension of ARRAY. If
!*  more than one element has the maximum value, the element whose subscripts are
!*  returned is the first such element, taken in array element order. If ARRAY has
!*  size zero, all elements of the result are zero.
!*   
!*  (322981/324275) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMaxloc6
  IMPLICIT NONE

  INTEGER     :: I
  INTEGER(1)  :: II1(2,2,2,2,2,2,2,2,2,2)  = 0 
  INTEGER(2)  :: II2(2,2,2,2,2,2,2,2,2,2)  = 0 
  INTEGER(4)  :: II4(2,2,2,2,2,2,2,2,2,2)  = 0 
  INTEGER(8)  :: II8(2,2,2,2,2,2,2,2,2,2)  = 0 

  REAL(4)     :: RR4(2,2,2,2,2,2,2,2,2,2)  = 0
  REAL(8)     :: RR8(2,2,2,2,2,2,2,2,2,2)  = 0
  REAL(16)    :: RR6(2,2,2,2,2,2,2,2,2,2)  = 0

  CHARACTER   :: CC(2,2,2,2,2,2,2,2,2,2)   = ACHAR(0)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(128)=.TRUE.
  REAL         :: R(128,128)= 1.1


  II1 = 0
  II1(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II1, KIND=II1%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 10
  IF (KIND(MAXLOC(ARRAY=II1, KIND=II1%KIND )) .NE. 1)                        STOP 11

  II1 = 0
  II1(2,1,2,2,2,2,2,2,2,2) = 1
  II1(1,2,2,2,2,2,2,2,2,2) = 1
  IF (ANY( MAXLOC(ARRAY=II1, KIND=II1%KIND )  .NE. (/2,1,2,2,2,2,2,2,2,2/))) STOP 12
  IF (KIND(MAXLOC(ARRAY=II1, KIND=II1%KIND )) .NE. 1)                        STOP 13

  II2 = 0
  II2(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II2, KIND=II2%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 14
  IF (KIND(MAXLOC(ARRAY=II2, KIND=II2%KIND )) .NE. 2)                        STOP 15

  II4 = 0
  II4(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II4, KIND=II4%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 16
  IF (KIND(MAXLOC(ARRAY=II4, KIND=II4%KIND )) .NE. 4)                        STOP 17

  II8 = 0
  II8(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II8, KIND=II8%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 18
  IF (KIND(MAXLOC(ARRAY=II8, KIND=II8%KIND )) .NE. 8)                        STOP 19

  II8 = 0
  II8(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II8 )                 .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 118
  IF (KIND(MAXLOC(ARRAY=II8 ))                .NE. 4)                        STOP 119

  RR4 = 0
  RR4(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) STOP 21
  IF (KIND(MAXLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)                        STOP 22

  RR8 = 0
  RR8(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) STOP 23
  IF (KIND(MAXLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)                        STOP 24

  RR6 = 0
  RR6(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-8 )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) STOP 25
  IF (KIND(MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-8 )) .NE. 8)                        STOP 26

  RR6 = 0
  RR6(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:) )                                      .NE. (/2,1,2,1,2,1,2,1,2,1/))) STOP 27
  IF (KIND(MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:) ))                                     .NE. 4)                        STOP 28


  CC = ACHAR(0) 
  CC(1,2,1,2,1,2,1,2,1,2) = ACHAR(1) 
  IF (ANY( MAXLOC(ARRAY=CC, KIND=CC%KIND ) .NE. (/1,2,1,2,1,2,1,2,1,2/)))   STOP 30
  IF (KIND(MAXLOC(ARRAY=CC, KIND=CC%KIND )).NE. 1)                          STOP 31
  IF (ANY( MAXLOC(ARRAY=CC )               .NE. (/1,2,1,2,1,2,1,2,1,2/)))   STOP 32
  IF (KIND(MAXLOC(ARRAY=CC ))              .NE. 4)                          STOP 33


  II1 = 0
  II1(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II1(1:0,:,:,:,:,:,:,:,:,:), KIND=II1%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 40
  IF (KIND(MAXLOC(ARRAY=II1(1:0,:,:,:,:,:,:,:,:,:), KIND=II1%KIND )) .NE. 1)                        STOP 41

  II2 = 0
  II2(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II2(:,1:0,:,:,:,:,:,:,:,:), KIND=II2%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 42
  IF (KIND(MAXLOC(ARRAY=II2(:,1:0,:,:,:,:,:,:,:,:), KIND=II2%KIND )) .NE. 2)                        STOP 43

  II4 = 0
  II4(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II4(:,:,1:0,:,:,:,:,:,:,:), KIND=II4%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 44
  IF (KIND(MAXLOC(ARRAY=II4(:,:,1:0,:,:,:,:,:,:,:), KIND=II4%KIND )) .NE. 4)                        STOP 45

  II8 = 0
  II8(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II8(:,:,:,1:0,:,:,:,:,:,:), KIND=II1%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 46
  IF (KIND(MAXLOC(ARRAY=II8(:,:,:,1:0,:,:,:,:,:,:), KIND=II1%KIND )) .NE. 1)                        STOP 47

  II8 = 0
  II8(1,2,1,2,1,2,1,2,1,2) = 1
  IF (ANY( MAXLOC(ARRAY=II8(:,:,:,:,1:0,:,:,:,:,:) )                 .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 48
  IF (KIND(MAXLOC(ARRAY=II8(:,:,:,:,1:0,:,:,:,:,:) ))                .NE. 4)                        STOP 49

  RR4 = 0
  RR4(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR4(:,:,:,:,:,1:0,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 51
  IF (KIND(MAXLOC(ARRAY=RR4(:,:,:,:,:,1:0,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)                        STOP 52

  RR8 = 0
  RR8(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR8(:,:,:,:,:,:,1:0,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 53
  IF (KIND(MAXLOC(ARRAY=RR8(:,:,:,:,:,:,1:0,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)                        STOP 54

  RR6 = 0
  RR6(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,1:0,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-8 )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 55
  IF (KIND(MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,1:0,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-8 )) .NE. 8)                        STOP 56

  RR6 = 0
  RR6(2,1,2,1,2,1,2,1,2,1) = 1
  IF (ANY( MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,1:0,:) )                                      .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 57
  IF (KIND(MAXLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,1:0,:) ))                                     .NE. 4)                        STOP 58


  CC = ACHAR(0) 
  CC(1,2,1,2,1,2,1,2,1,2) = ACHAR(1) 
  IF (ANY( MAXLOC(ARRAY=CC(:,:,:,:,:,:,:,:,:,1:0), KIND=CC%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/)))   STOP 60
  IF (KIND(MAXLOC(ARRAY=CC(:,:,:,:,:,:,:,:,:,1:0), KIND=CC%KIND )) .NE. 1)                          STOP 61
  IF (ANY( MAXLOC(ARRAY=CC(:,:,:,:,1:0,:,:,:,:,:) )                .NE. (/0,0,0,0,0,0,0,0,0,0/)))   STOP 62
  IF (KIND(MAXLOC(ARRAY=CC(:,:,:,:,1:0,:,:,:,:,:) ))               .NE. 4)                          STOP 63



  END
