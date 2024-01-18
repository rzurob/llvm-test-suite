!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim2
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
!*  characteristics :: the keyword - KIND 
!*
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim2

  INTEGER(1),    PARAMETER :: I(3) = (/1,1,1/) 
  CHARACTER,     PARAMETER :: C(3) = (/"I","B","M"/) 
  INTEGER,       PARAMETER :: Kind(2)=(/1,2/)
 
  PRINT*, LEN_TRIM(STRING=C(1), KINDD=I(1))
  PRINT*, LEN_TRIM(STRING=C(2), KID=Kind(1))
  PRINT*, LEN_TRIM(KIND=I(1), KIND=Kind(2))
  PRINT*, LEN_TRIM(STRING=(/"I","B","M"/), KIND=1, STRING=(/"I","B","M"/))
  PRINT*, LEN_TRIM(LEN_TRIM=.TRUE., STRING=(/"1","2"/))

  PRINT*, LEN_TRIM(StrinG=C, kInD=I(2)+I(1)) ! this is fine
  PRINT*, LEN_TRIM(string=C, kInD=C(:)%KIND) ! this is fine

  END

