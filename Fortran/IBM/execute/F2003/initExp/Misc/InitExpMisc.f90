!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpMisc.f  
!*  TESTOP CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Sept. 11 2006
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Charber 289074 
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
!*  Misc  on pack 
!* 
!* (325095) 
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0
    INTEGER :: ID
  CONTAINS
  PROCEDURE :: MyAssgn
  GENERIC :: ASSIGNMENT(=) => MyAssgn
  END TYPE

  CONTAINS

  ELEMENTAL SUBROUTINE MyAssgn(Arg1, Arg2)
  CLASS(DT0), INTENT(INOUT) :: Arg1
  TYPE(DT0), INTENT(IN)    :: Arg2
    Arg1%ID = -Arg2%ID
  END SUBROUTINE

  END MODULE

  PROGRAM InitExpMisc
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT
    TYPE(DT0) :: T
  END TYPE
 

  TYPE (DT) :: T(64)=[(                                         &
                         PACK([(DT(DT0(I)),J=1,I-1)], .TRUE.),  &
                         DT(DT0(-I)),                           &
                         PACK([(DT(DT0(I)),J=I+1,8)], .TRUE.),  &
                       I=1,8)]

  DO I=1, 8
      IF (ANY( T(8*(I-1)+1:9*(I-1))%T%ID .NE. [(J,J=1,I-1)]))  STOP 11
      IF (T(9*I-8)%T%ID                  .NE. -I )             STOP 12
      IF (ANY( T(9*(i-1)+2:8*I)%T%ID     .NE. [(J,J=I+1,8)]))  STOP 13
  END DO

  END 


 
