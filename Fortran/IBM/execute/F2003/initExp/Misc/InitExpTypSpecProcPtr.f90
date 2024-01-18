!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecProcPtr.f  
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
!*  intrinsic-type-spec in proc ptr: integer and logical 
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION I1()
    INTEGER(1) :: I1
    I1 = 1 
  END FUNCTION
 
  FUNCTION I2()
    INTEGER(2) :: I2
    I2 = 2 
  END FUNCTION
 
  FUNCTION I4()
    INTEGER(4) :: I4
    I4 = 4 
  END FUNCTION
 
  FUNCTION I8()
    INTEGER(8) :: I8
    I8 = 8 
  END FUNCTION
 
 
  FUNCTION L1()
    LOGICAL(1) :: L1
    L1 = .TRUE.
  END FUNCTION
 
  FUNCTION L2()
    LOGICAL(2) :: L2
    L2 = .TRUE.
  END FUNCTION
 
  FUNCTION L4()
    LOGICAL(4) :: L4
    L4 = .TRUE.
  END FUNCTION
 
  FUNCTION L8()
    LOGICAL(8) :: L8
    L8 = .TRUE.
  END FUNCTION
 
  END MODULE


  PROGRAM InitExpTypSpecProcPtr 
  USE M
  IMPLICIT NONE

  INTEGER :: I

  
  PROCEDURE(INTEGER(KIND=1_8)),             POINTER :: I1ProcPtr
  PROCEDURE(INTEGER(KIND=KIND([1_1])+1)),   POINTER :: I2ProcPtr
  PROCEDURE(INTEGER(KIND=KIND([1_2])*2)),   POINTER :: I4ProcPtr
  PROCEDURE(INTEGER(KIND=KIND([1_8])*1)),   POINTER :: I8ProcPtr

  PROCEDURE(LOGICAL(KIND=KIND([1_1]))),     POINTER :: L1ProcPtr
  PROCEDURE(LOGICAL(KIND=KIND([1_2]))),     POINTER :: L2ProcPtr
  PROCEDURE(LOGICAL(KIND=KIND([1_4]))),     POINTER :: L4ProcPtr
  PROCEDURE(LOGICAL(KIND=KIND([1_8]))),     POINTER :: L8ProcPtr

  I1ProcPtr => I1
  I2ProcPtr => I2
  I4ProcPtr => I4
  I8ProcPtr => I8

  L1ProcPtr => L1
  L2ProcPtr => L2
  L4ProcPtr => L4
  L8ProcPtr => L8


  IF ( I1ProcPtr() .NE. 1      ) STOP 11
  IF ( I2ProcPtr() .NE. 2      ) STOP 12
  IF ( I4ProcPtr() .NE. 4      ) STOP 13
  IF ( I8ProcPtr() .NE. 8      ) STOP 14

  IF ( L1ProcPtr() .NEQV. .TRUE. ) STOP 21
  IF ( L2ProcPtr() .NEQV. .TRUE. ) STOP 22
  IF ( L4ProcPtr() .NEQV. .TRUE. ) STOP 23
  IF ( L8ProcPtr() .NEQV. .TRUE. ) STOP 24

  END

 
