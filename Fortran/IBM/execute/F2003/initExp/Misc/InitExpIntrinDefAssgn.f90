!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpIntrinDefAssgn.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 22, 2006
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
!*  Defined assignment( for DT) shall not have any impact onto entity of DT 
!* 
!*  
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    INTEGER :: Id=0
  CONTAINS
    PROCEDURE :: Assign => MyAssign
    GENERIC   :: ASSIGNMENT(=) => Assign 
  END TYPE

  CONTAINS

  ELEMENTAL SUBROUTINE MyAssign(Arg1, Arg2)
  CLASS(DT), INTENT(INOUT):: Arg1
  TYPE(DT),  INTENT(IN)   :: Arg2
    Arg1%Id = - Arg2%Id 
  END SUBROUTINE

  END MODULE

  PROGRAM InitExpIntrinDefAssgn 
  USE M
  IMPLICIT NONE
 
  INTEGER  :: I
  TYPE(DT) :: T=DT(1)
  TYPE(DT) :: TArr(128)=(/(DT(I), I=0,127)/)

 
  IF (T%Id        .NE. 1  )               STOP 11
  IF (ANY(TArr%Id .NE. (/(I, I=0,127)/))) STOP 12


  END

 
