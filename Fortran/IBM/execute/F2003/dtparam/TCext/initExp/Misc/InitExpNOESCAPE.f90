! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/initExp/Misc/InitExpNOESCAPE.f
! opt variations: -qck -qnok

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

  TYPE :: DT(K1,N1)    ! (4,3)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: C= "\\\"
  END TYPE

  TYPE(DT(4,3)) :: T

  IF ( ANY( C  .NE. "\"//"t"//"\" ))     STOP 12
  IF ( T%C     .NE. "\"//"\"//"\" )      STOP 13

  END


