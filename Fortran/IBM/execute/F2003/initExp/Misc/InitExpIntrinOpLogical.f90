!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          BM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nitExpIntrinOpLogical.f  
!*  TEST CASE TTLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 29, 2006
!*  ORGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRVER STANZA              :
!*  REQURED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDTIONS :
!*
!*  DESCRPTION
!*
!*  
!*  .NOT. 
!*  .AND., .OR., .EQV., .NEQV. 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM nitExpIntrinOpLogical 
  IMPLICIT NONE

  INTEGER :: I

  LOGICAL(1), PARAMETER :: L1(128) = .TRUE. 
  LOGICAL(2), PARAMETER :: L2(128) = .NOT. L1 
  LOGICAL(4), PARAMETER :: L4(128) = .FALSE. 
  LOGICAL(8), PARAMETER :: L8(128) = L1 

  LOGICAL(1), PARAMETER :: TL1(128) = L1 .AND. L4 
  LOGICAL(2), PARAMETER :: TL2(128) = L4 .OR. L8 
  LOGICAL(4), PARAMETER :: TL4(128) = L2 .EQV. L1 
  LOGICAL(8), PARAMETER :: TL8(128) = L1 .NEQV. L2 

  IF ( ANY(TL1  .NEQV. .FALSE.) ) STOP 11
  IF ( ANY(TL2  .NEQV. .TRUE. ) ) STOP 12
  IF ( ANY(TL4  .NEQV. .FALSE.) ) STOP 13
  IF ( ANY(TL8  .NEQV. .TRUE. ) ) STOP 14


  END

 
