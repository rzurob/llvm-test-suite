!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMinloc7
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
!*  Case (ii): The result of MINLOC (ARRAY, MASK = MASK) is a rank-one array whose
!*  element values are the values of the subscripts of an element of ARRAY, corre22
!*  sponding to a true element of MASK, whose value equals the minimum value of
!*  all such elements of ARRAY. The ith subscript returned lies in the range 1 to
!*  ei, where ei is the extent of the ith dimension of ARRAY. If more than one such
!*  element has the minimum value, the element whose subscripts are returned is the
!*  first such element taken in array element order. If ARRAY has size zero or every
!*  element of MASK has the value false, all elements of the result are zero.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMinloc7
  IMPLICIT NONE

  INTEGER     :: I
  INTEGER(1)  :: II1(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)  = 0
  INTEGER(2)  :: II2(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)  = 0
  INTEGER(4)  :: II4(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)  = 0
  INTEGER(8)  :: II8(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)  = 0

  REAL(4)     :: RR4(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)  = 0
  REAL(8)     :: RR8(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)  = 0
  REAL(16)    :: RR6(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)  = 0

  CHARACTER   :: CC(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)   = ACHAR(0)

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  LOGICAL      :: MM(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)=.TRUE.
  LOGICAL      :: MM1(-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0,-1:0)=.FALSE.


  II1 = 1
  II1(:,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II1, MASK=MM, KIND=II1%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 10
  IF (KIND(MINLOC(ARRAY=II1, MASK=MM, KIND=II1%KIND )) .NE. 1)                        STOP 11

  II2 = 1
  II2(-1,:,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II2, MASK=MM, KIND=II2%KIND )  .NE. (/1,1,1,2,1,2,1,2,1,2/))) STOP 12
  IF (KIND(MINLOC(ARRAY=II2, MASK=MM, KIND=II2%KIND )) .NE. 2)                        STOP 13

  II4 = 1
  II4(-1,0,:,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II4, MASK=MM, KIND=II4%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 14
  IF (KIND(MINLOC(ARRAY=II4, MASK=MM, KIND=II4%KIND )) .NE. 4)                        STOP 15

  II8 = 1
  II8(-1,0,-1,:,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II8, MASK=MM, KIND=II1%KIND )  .NE. (/1,2,1,1,1,2,1,2,1,2/))) STOP 16
  IF (KIND(MINLOC(ARRAY=II8, MASK=MM, KIND=II1%KIND )) .NE. 1)                        STOP 17

  II8 = 1
  II8(-1,0,-1,0,:,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II8 )                 .NE. (/1,2,1,2,1,2,1,2,1,2/))) STOP 18
  IF (KIND(MINLOC(ARRAY=II8 ))                .NE. 4)                        STOP 19

  RR4 = 1
  RR4(0,-1,0,-1,0,:,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), MASK=MM, KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) STOP 21
  IF (KIND(MINLOC(ARRAY=RR4(:,:,:,:,:,:,:,:,:,:), MASK=MM, KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)                        STOP 22

  RR8 = 1
  RR8(0,-1,0,-1,0,-1,:,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), MASK=MM, KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/2,1,2,1,2,1,1,1,2,1/))) STOP 23
  IF (KIND(MINLOC(ARRAY=RR8(:,:,:,:,:,:,:,:,:,:), MASK=MM, KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)                        STOP 24

  RR6 = 1
  RR6(0,-1,0,-1,0,-1,0,:,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), MASK=MM, KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-14 )  .NE. (/2,1,2,1,2,1,2,1,2,1/))) STOP 25
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:), MASK=MM, KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-14 )) .NE.  2)                       STOP 26

  RR6 = 1
  RR6(0,-1,0,-1,0,-1,0,-1,:,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:) )                                      .NE. (/2,1,2,1,2,1,2,1,1,1/))) STOP 27
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,:,:) ))                                     .NE. 4)                        STOP 28


  CC = ACHAR(1)
  CC(-1,0,-1,0,-1,0,-1,0,-1,:) = ACHAR(0)
  IF (ANY( MINLOC(ARRAY=CC, MASK=MM, KIND=CC%KIND )  .NE. (/1,2,1,2,1,2,1,2,1,1/)))      STOP 30
  IF (KIND(MINLOC(ARRAY=CC, MASK=MM, KIND=CC%KIND )) .NE. 1)                             STOP 31
  IF (ANY( MINLOC(ARRAY=CC )                         .NE. (/1,2,1,2,1,2,1,2,1,1/)))      STOP 32
  IF (KIND(MINLOC(ARRAY=CC ))                        .NE. 4)                             STOP 33


  II1 = 1
  II1(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II1(1:0,:,:,:,:,:,:,:,:,:), MASK=MM(1:0,:,:,:,:,:,:,:,:,:), KIND=II1%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 40
  IF (KIND(MINLOC(ARRAY=II1(1:0,:,:,:,:,:,:,:,:,:), MASK=MM(1:0,:,:,:,:,:,:,:,:,:), KIND=II1%KIND )) .NE. 1)                        STOP 41

  II2 = 1
  II2(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II2(:,1:0,:,:,:,:,:,:,:,:), MASK=MM(:,1:0,:,:,:,:,:,:,:,:), KIND=II2%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 42
  IF (KIND(MINLOC(ARRAY=II2(:,1:0,:,:,:,:,:,:,:,:), MASK=MM(:,1:0,:,:,:,:,:,:,:,:), KIND=II2%KIND )) .NE. 2)                        STOP 43

  II4 = 1
  II4(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II4(:,:,1:0,:,:,:,:,:,:,:), MASK=MM(:,:,1:0,:,:,:,:,:,:,:), KIND=II4%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 44
  IF (KIND(MINLOC(ARRAY=II4(:,:,1:0,:,:,:,:,:,:,:), MASK=MM(:,:,1:0,:,:,:,:,:,:,:), KIND=II4%KIND )) .NE. 4)                        STOP 45

  II8 = 1
  II8(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II8(:,:,:,1:0,:,:,:,:,:,:), MASK=MM(:,:,:,1:0,:,:,:,:,:,:), KIND=II8%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 46
  IF (KIND(MINLOC(ARRAY=II8(:,:,:,1:0,:,:,:,:,:,:), MASK=MM(:,:,:,1:0,:,:,:,:,:,:), KIND=II8%KIND )) .NE. 8)                        STOP 47

  II8 = 1
  II8(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II8(:,:,:,:,1:0,:,:,:,:,:) )                 .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 48
  IF (KIND(MINLOC(ARRAY=II8(:,:,:,:,1:0,:,:,:,:,:) ))                .NE. 4)                        STOP 49

  RR4 = 1
  RR4(0,-1,0,-1,0,-1,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR4(:,:,:,:,:,1:0,:,:,:,:), MASK=MM(:,:,:,:,:,1:0,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 51
  IF (KIND(MINLOC(ARRAY=RR4(:,:,:,:,:,1:0,:,:,:,:), MASK=MM(:,:,:,:,:,1:0,:,:,:,:), KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)                        STOP 52

  RR8 = 1
  RR8(0,-1,0,-1,0,-1,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR8(:,:,:,:,:,:,1:0,:,:,:), MASK=MM(:,:,:,:,:,:,1:0,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 53
  IF (KIND(MINLOC(ARRAY=RR8(:,:,:,:,:,:,1:0,:,:,:), MASK=MM(:,:,:,:,:,:,1:0,:,:,:), KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)                        STOP 54

  RR6 = 1
  RR6(0,-1,0,-1,0,-1,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,1:0,:,:), MASK=MM(:,:,:,:,:,:,:,1:0,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-8 )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 55
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,1:0,:,:), MASK=MM(:,:,:,:,:,:,:,1:0,:,:), KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-8 )) .NE.  8)                       STOP 56

  RR6 = 1
  RR6(0,-1,0,-1,0,-1,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,1:0,:) )                                      .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 57
  IF (KIND(MINLOC(ARRAY=RR6(:,:,:,:,:,:,:,:,1:0,:) ))                                     .NE. 4)                        STOP 58


  CC = ACHAR(1)
  CC(-1,0,-1,0,-1,0,-1,0,-1,0) = ACHAR(0)
  IF (ANY( MINLOC(ARRAY=CC(:,:,:,:,:,:,:,:,:,1:0), MASK=.FALSE., KIND=CC%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/)))   STOP 60
  IF (KIND(MINLOC(ARRAY=CC(:,:,:,:,:,:,:,:,:,1:0), MASK=.FALSE., KIND=CC%KIND )) .NE. 1)                          STOP 61
  IF (ANY( MINLOC(ARRAY=CC(:,:,:,:,1:0,:,:,:,:,:) )                .NE. (/0,0,0,0,0,0,0,0,0,0/)))   STOP 62
  IF (KIND(MINLOC(ARRAY=CC(:,:,:,:,1:0,:,:,:,:,:) ))               .NE. 4)                          STOP 63



  II1 = 1
  II1(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II1, MASK=MM1, KIND=II1%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 140
  IF (KIND(MINLOC(ARRAY=II1, MASK=MM1, KIND=II1%KIND )) .NE. 1)                        STOP 141

  II2 = 1
  II2(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II2, MASK=MM1, KIND=II2%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 142
  IF (KIND(MINLOC(ARRAY=II2, MASK=MM1, KIND=II2%KIND )) .NE. 2)                        STOP 143

  II4 = 1
  II4(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II4, MASK=MM1, KIND=II4%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 144
  IF (KIND(MINLOC(ARRAY=II4, MASK=MM1, KIND=II4%KIND )) .NE. 4)                        STOP 145

  II8 = 1
  II8(-1,0,-1,0,-1,0,-1,0,-1,0) = 0
  IF (ANY( MINLOC(ARRAY=II8, MASK=MM1, KIND=II8%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 146
  IF (KIND(MINLOC(ARRAY=II8, MASK=MM1, KIND=II8%KIND )) .NE. 8)                        STOP 147


  RR4 = 1
  RR4(0,-1,0,-1,0,-1,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR4, MASK=MM1, KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 151
  IF (KIND(MINLOC(ARRAY=RR4, MASK=MM1, KIND=RR4(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 4)                        STOP 152

  RR8 = 1
  RR8(0,-1,0,-1,0,-1,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR8, MASK=MM1, KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 153
  IF (KIND(MINLOC(ARRAY=RR8, MASK=MM1, KIND=RR8(:,:,:,:,:,:,:,:,:,:)%KIND )) .NE. 8)                        STOP 154

  RR6 = 1
  RR6(0,-1,0,-1,0,-1,0,-1,0,-1) = 0
  IF (ANY( MINLOC(ARRAY=RR6, MASK=MM1, KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-12 )  .NE. (/0,0,0,0,0,0,0,0,0,0/))) STOP 155
  IF (KIND(MINLOC(ARRAY=RR6, MASK=MM1, KIND=RR6(:,:,:,:,:,:,:,:,:,:)%KIND-12 )) .NE.  4)                       STOP 156



  CC = ACHAR(1)
  CC(-1,0,-1,0,-1,0,-1,0,-1,0) = ACHAR(0)
  IF (ANY( MINLOC(ARRAY=CC, MASK=MM1, KIND=CC%KIND )  .NE. (/0,0,0,0,0,0,0,0,0,0/)))   STOP 160
  IF (KIND(MINLOC(ARRAY=CC, MASK=MM1, KIND=CC%KIND )) .NE. 1)                          STOP 161



  END
