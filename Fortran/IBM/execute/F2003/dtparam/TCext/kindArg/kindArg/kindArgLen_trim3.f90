! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodeferredlp /tstdev/F2003/kindArg/kindArg/kindArgLen_trim3.f
! opt variations: -qnock -qnok -qnol -qdeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 23, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : LEN_TRIM 
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
!*  Returns the length of the character argument without counting trailing blank characters. 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim3

  TYPE :: DT(D1,N1)    ! (4,20)
    INTEGER, KIND :: D1
    INTEGER, LEN  :: N1
    CHARACTER(:), ALLOCATABLE :: CC(:,:)
  END TYPE
  
  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8
    
  TYPE(DT(4,20)), POINTER :: T
  INTEGER, PARAMETER :: II(128)=(/(I,I=1,128)/)
  INTEGER            :: III(128,128) 
 
  ALLOCATE(T)
  ALLOCATE(CHARACTER(128) :: T%CC(128,128))

  T%CC(:,:)=" "
  DO I=1,128
  DO J=1, 128
    T%CC(I,J)(J:J)="X"
    III(I,J)=J
  END DO
  END DO
 

  DO I1 =1, 127
    IF (LEN_TRIM(STRING=T%CC(I1,I1), KIND=I1%KIND )   .NE. I1 )   STOP 11
  END DO

  DO I2 =1, 128
    IF (ANY( LEN_TRIM(STRING=T%CC(I2,:)(1:), KIND=I2%KIND )   .NE. II ))    STOP 12
  END DO

  DO I4 =1, 128
    IF ( ANY (LEN_TRIM(STRING=T%CC(:,I4)(I4:), KIND=I4%KIND ) .NE. 1 )) STOP 14
  END DO

  DO I8 =1, 128
    IF (ANY( LEN_TRIM(STRING=T%CC(I8,:), KIND=I8%KIND ) .NE. II))     STOP 18
  END DO


  IF (ANY(LEN_TRIM(STRING=T%CC(:,:)(:))         .NE. III)) STOP 92
  END
