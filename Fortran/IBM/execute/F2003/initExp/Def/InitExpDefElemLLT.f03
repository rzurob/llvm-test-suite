!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an elemental intrinsic
!*
!*  -  LLT
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemLLT
  IMPLICIT NONE
  INTEGER :: I, J

  TYPE :: DT0
    INTEGER, POINTER     :: Ptr=>NULL()
    REAL,    ALLOCATABLE :: R
    CHARACTER(10)        :: C1=""
    CHARACTER(10)        :: C2=ACHAR(1)//ACHAR(2)//ACHAR(3)//ACHAR(4)//ACHAR(5)//   &
                               ACHAR(6)//ACHAR(7)//ACHAR(8)//ACHAR(9)//ACHAR(0)
    CHARACTER(2**8-1)    :: C3=CHAR(0)
    PROCEDURE(), POINTER, NOPASS :: ProcPtr =>NULL()
  END TYPE

  TYPE(DT0), PARAMETER :: C(16)=DT0(R=NULL())

  TYPE :: DT
    LOGICAL :: C1(16)=LLT(STRING_B=(/C%C1/), STRING_A=(/C%C2(1:0)/))
    LOGICAL :: C2(16)=LLT(STRING_B=(/C%C1/), STRING_A=(/C%C2(10:10)/))
    LOGICAL :: C3(16)=LLT(STRING_B=(/C%C2/), STRING_A=(/C%C2(1:9)/))
    LOGICAL :: C4(16)=LLT(STRING_B=(/C%C2//C%C1/), STRING_A=(/C%C2(:9)//C%C2(1:0)/))
    LOGICAL :: C5(16)=LLT(STRING_B=(/C%C2//C%C3/), STRING_A=(/C%C2(:)//C%C3(:9)/))
    LOGICAL :: C6(16)=LLT(STRING_B=(/C%C1//C%C3/), STRING_A=(/C%C1(:)//C%C3(1:1)/))
    LOGICAL :: C7(16)=LLT(STRING_B=(/C%C2(1:6)/),  STRING_A=(/C%C2(1:6)/))
  END TYPE

  TYPE(DT) :: T

  IF( ANY( T%C1  .NEQV. .FALSE. ))       ERROR STOP 11
  IF( ANY( T%C2  .NEQV. .TRUE.  ))       ERROR STOP 12
  IF( ANY( T%C3  .NEQV. .FALSE. ))       ERROR STOP 13
  IF( ANY( T%C4  .NEQV. .FALSE. ))       ERROR STOP 14
  IF( ANY( T%C5  .NEQV. .FALSE. ))       ERROR STOP 15
  IF( ANY( T%C6  .NEQV. .FALSE. ))       ERROR STOP 16
  IF( ANY( T%C7  .NEQV. .FALSE. ))       ERROR STOP 17


  END

