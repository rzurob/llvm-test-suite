!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpNOESCAPE.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 25, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qrealsize 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  -qnoescape 
!* 
!* 
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpNOESCAPE
  IMPLICIT NONE

  INTEGER  :: I 

  CHARACTER(3),    PARAMETER :: C(128)=(/( "\t\", I=0, 127)/)  

  TYPE :: DT
    CHARACTER(3) :: C= "\\\"
  END TYPE

  TYPE(DT) :: T

  IF ( ANY( C  .NE. "\"//"t"//"\" ))     STOP 12
  IF ( T%C     .NE. "\"//"\"//"\" )      STOP 13

  END


