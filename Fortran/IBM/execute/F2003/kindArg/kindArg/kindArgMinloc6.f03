!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MINLOC
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Result Value.
!*  Case (i): The result of MINLOC (ARRAY) is a rank-one array whose element values are
!*  the values of the subscripts of an element of ARRAY whose value equals the
!*  minimum value of all the elements of ARRAY. The ith subscript returned lies
!*  in the range 1 to ei, where ei is the extent of the ith dimension of ARRAY. If
!*  more than one element has the minimum value, the element whose subscripts are
!*  returned is the first such element, taken in array element order. If ARRAY has
!*  size zero, all elements of the result are zero.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMinloc6
  IMPLICIT NONE

  INTEGER     :: I
  INTEGER(1)  :: II1(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)  = 0
  INTEGER(2)  :: II2(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)  = 0
  INTEGER(4)  :: II4(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)  = 0
  INTEGER(8)  :: II8(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)  = 0

  REAL(4)     :: RR4(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)  = 0
  REAL(8)     :: RR8(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)  = 0
  REAL(16)    :: RR6(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)  = 0

  CHARACTER   :: CC(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)   = ACHAR(0)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(128)=.TRUE.
  REAL         :: R(128,128)= 1.1


  II1 = 1
  II1(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II1, KIND=II1%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) ERROR STOP 8
  IF (KIND(MINLOC(ARRAY=II1, KIND=II1%KIND )) .NE. 1)                        ERROR STOP 9

  II1 = 1
  II1(1,0,1,1,1,1,1,1,1,1) = 0
  II1(0,1,1,1,1,1,1,1,1,1) = 0
  IF (ANY( MINLOC(ARRAY=II1, KIND=II1%KIND )  .NE. (/2,1,2,2,2,2,2,2,2,2/))) ERROR STOP 10
  IF (KIND(MINLOC(ARRAY=II1, KIND=II1%KIND )) .NE. 1)                        ERROR STOP 11

  II2 = 1
  II2(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II2, KIND=II2%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) ERROR STOP 12
  IF (KIND(MINLOC(ARRAY=II2, KIND=II2%KIND )) .NE. 2)                        ERROR STOP 13

  II4 = 1
  II4(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II4, KIND=II4%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) ERROR STOP 14
  IF (KIND(MINLOC(ARRAY=II4, KIND=II4%KIND )) .NE. 4)                        ERROR STOP 15

  II8 = 1
  II8(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II8, KIND=II8%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) ERROR STOP 16
  IF (KIND(MINLOC(ARRAY=II8, KIND=II8%KIND )) .NE. 8)                        ERROR STOP 17

  II8 = 1
  II8(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II8 )                 .NE. (/1,2,1,2,1,2,1,2,1,2/))) ERROR STOP 18
  IF (KIND(MINLOC(ARRAY=II8 ))                .NE. 4)                        ERROR STOP 19

  RR4 = 1
  RR4(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) ERROR STOP 21
  IF (KIND(MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)                        ERROR STOP 22

  RR8 = 1
  RR8(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) ERROR STOP 23
  IF (KIND(MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)                        ERROR STOP 24

  RR6 = 1
  RR6(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-12 )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) ERROR STOP 25
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-12 )) .NE.  4)                       ERROR STOP 26

  RR6 = 1
  RR6(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:) )                                      .NE. (/2,1,2,1,2,1,2,1,2,1/))) ERROR STOP 27
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:) ))                                     .NE. 4)                        ERROR STOP 28

  CC = ACHAR(1)
  CC(0,1,0,1,0,1,0,1,0,1) = ACHAR(0)
  IF (ANY( MINLOC(ARRAY=CC, KIND=CC%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/)))   ERROR STOP 30
  IF (KIND(MINLOC(ARRAY=CC, KIND=CC%KIND )) .NE. 1)                          ERROR STOP 31
  IF (ANY( MINLOC(ARRAY=CC )                .NE. (/1,2,1,2,1,2,1,2,1,2/)))   ERROR STOP 32
  IF (KIND(MINLOC(ARRAY=CC ))               .NE. 4)                          ERROR STOP 33


  II1 = 1
  II1(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II1(1:0,:,:,:,:,:,:,:,:,:), KIND=II1%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 40
  IF (KIND(MINLOC(ARRAY=II1(1:0,:,:,:,:,:,:,:,:,:), KIND=II1%KIND )) .NE. 1)                        ERROR STOP 41

  II2 = 1
  II2(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II2(:,1:0,:,:,:,:,:,:,:,:), KIND=II2%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 42
  IF (KIND(MINLOC(ARRAY=II2(:,1:0,:,:,:,:,:,:,:,:), KIND=II2%KIND )) .NE. 2)                        ERROR STOP 43

  II4 = 1
  II4(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II4(:,:,1:0,:,:,:,:,:,:,:), KIND=II4%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 44
  IF (KIND(MINLOC(ARRAY=II4(:,:,1:0,:,:,:,:,:,:,:), KIND=II4%KIND )) .NE. 4)                        ERROR STOP 45

  II8 = 1
  II8(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II8(:,:,:,1:0,:,:,:,:,:,:), KIND=II1%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 46
  IF (KIND(MINLOC(ARRAY=II8(:,:,:,1:0,:,:,:,:,:,:), KIND=II1%KIND )) .NE. 1)                        ERROR STOP 47

  II8 = 1
  II8(0,1,0,1,0,1,0,1,0,1) = 0
  IF (ANY( MINLOC(ARRAY=II8(:,:,:,:,1:0,:,:,:,:,:) )                 .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 48
  IF (KIND(MINLOC(ARRAY=II8(:,:,:,:,1:0,:,:,:,:,:) ))                .NE. 4)                        ERROR STOP 49

  RR4 = 1
  RR4(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR4(:,:,:,:,:,1:0,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 51
  IF (KIND(MINLOC(ARRAY=RR4(:,:,:,:,:,1:0,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)                        ERROR STOP 52

  RR8 = 1
  RR8(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR8(:,:,:,:,:,:,1:0,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 53
  IF (KIND(MINLOC(ARRAY=RR8(:,:,:,:,:,:,1:0,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)                        ERROR STOP 54

  RR6 = 1
  RR6(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,1:0,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-15 )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 55
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,1:0,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-15 )) .NE. 1 )                       ERROR STOP 56

  RR6 = 1
  RR6(1,0,1,0,1,0,1,0,1,0) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,1:0,:) )                                      .NE. (/0,0,0,0,0,0,0,0,0,0/))) ERROR STOP 57
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,1:0,:) ))                                     .NE. 4)                        ERROR STOP 58


  CC = ACHAR(1)
  CC(0,1,0,1,0,1,0,1,0,1) = ACHAR(0)
  IF (ANY( MINLOC(ARRAY=CC(:,:,:,:,:,:,:,:,:,1:0), KIND=CC%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/)))   ERROR STOP 60
  IF (KIND(MINLOC(ARRAY=CC(:,:,:,:,:,:,:,:,:,1:0), KIND=CC%KIND )) .NE. 1)                          ERROR STOP 61
  IF (ANY( MINLOC(ARRAY=CC(:,:,:,:,1:0,:,:,:,:,:) )                .NE. (/0,0,0,0,0,0,0,0,0,0/)))   ERROR STOP 62
  IF (KIND(MINLOC(ARRAY=CC(:,:,:,:,1:0,:,:,:,:,:) ))               .NE. 4)                          ERROR STOP 63



  END
