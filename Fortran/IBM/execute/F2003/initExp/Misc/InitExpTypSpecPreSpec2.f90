!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecPreSpec2.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 29, 2006
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
!*  intrinsic-type-spec in Prefix-spec : char 
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  !CHARACTER(LEN=C%LEN, KIND=C%KIND) FUNCTION C1()
  !  CHARACTER, POINTER :: C(:)
  CHARACTER(LEN=1, KIND=1) FUNCTION C1()
    C1 = "1" 
  END FUNCTION
 
  CHARACTER(LEN=1+1,  KIND=1) FUNCTION C2()
    C2 = "12" 
  END FUNCTION
 
  CHARACTER(1+2,  KIND=1) FUNCTION C3()
    CHARACTER, POINTER :: C(:)
    C3 = "123" 
  END FUNCTION
 


  PROGRAM InitExpTypSpecPreSpec2 
  IMPLICIT NONE

  INTEGER :: I
  CHARACTER, POINTER :: C(:)

  PROCEDURE(CHARACTER(KIND=C%KIND)),                POINTER :: C1PreSpec
  PROCEDURE(CHARACTER(LEN=C%LEN+1,  KIND=C%KIND)),  POINTER :: C2PreSpec
  PROCEDURE(CHARACTER(C%LEN+2,  KIND=C%KIND)),      POINTER :: C3PreSpec

  PROCEDURE(CHARACTER(KIND=C%KIND)),                EXTERNAL :: C1
  PROCEDURE(CHARACTER(LEN=C%LEN+1,  KIND=C%KIND)),  EXTERNAL :: C2
  PROCEDURE(CHARACTER(C%LEN+2,  KIND=C%KIND)),      EXTERNAL :: C3


  C1PreSpec => C1 
  C2PreSpec => C2 
  C3PreSpec => C3 


  IF ( C1PreSpec() .NE. "1"    ) STOP 11
  IF ( C2PreSpec() .NE. "12"   ) STOP 12
  IF ( C3PreSpec() .NE. "123"  ) STOP 13

  END

 
