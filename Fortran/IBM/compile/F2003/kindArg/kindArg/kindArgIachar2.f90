!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIachar2
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR 
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


  PROGRAM kindArgIachar2

  INTEGER(1),    PARAMETER :: I(3) = (/1,1,1/) 
  CHARACTER,     PARAMETER :: C(3) = (/"I","B","M"/) 
  
  PRINT*, IACHAR(ACHAR(1_1), KINDD=I(1))
  PRINT*, IACHAR(C="1", KID=kind(1))
  PRINT*, IACHAR(KIND=I(1), KIND=2)
  PRINT*, IACHAR(C=(/10,11/), KIND=1, KIND=1)
  PRINT*, IACHAR(KIND=KIND(C), C=(/"1","2"/), KIND=1_2)

  PRINT*, IACHAR(C=ACHAR(IACHAR("A")), kInD=I(2)) ! this is fine
  PRINT*, IACHAR(C=C, kInD=KIND(C)) ! this is fine

  END

