!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 11, 2006
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
!*  -  IS_IOSTAT_EOR
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIS_IOSTAT_EOR
  IMPLICIT  NONE
  INTEGER :: I, J

  INTEGER(1), PARAMETER :: IEndOfRecord1(4)=(/1,0,-1,-4/)
  INTEGER(2), PARAMETER :: IEndOfRecord2(4)=(/1,0,-1,-4/)
  INTEGER(4), PARAMETER :: IEndOfRecord4(4)=(/1,0,-1,-4/)
  INTEGER(8), PARAMETER :: IEndOfRecord8(4)=(/1,0,-1,-4/)
  LOGICAL,    PARAMETER :: LEndOfRecord(4)=(/.FALSE.,.FALSE.,.FALSE.,.TRUE./)

  LOGICAL(KIND(IS_IOSTAT_EOR(IEndOfRecord1) )), PARAMETER :: L1(4)=IS_IOSTAT_EOR(I=IEndOfRecord1)
  LOGICAL(KIND(IS_IOSTAT_EOR(IEndOfRecord2) )), PARAMETER :: L2(4)=IS_IOSTAT_EOR(IEndOfRecord2)
  LOGICAL(KIND(IS_IOSTAT_EOR(IEndOfRecord4) )), PARAMETER :: L4(4)=IS_IOSTAT_EOR(i=IEndOfRecord4)
  LOGICAL(KIND(IS_IOSTAT_EOR(IEndOfRecord8) )), PARAMETER :: L8(4)=IS_IOSTAT_EOR(IEndOfRecord8)

  LOGICAL, PARAMETER :: L0(1:0)=IS_IOSTAT_EOR(IEndOfRecord8(1:0))


  IF( KIND(L1) .NE. 4 )                 ERROR STOP 11
  IF(ANY(  L1 .NEQV. LEndOfRecord  ))   ERROR STOP 12
  IF( KIND(L2) .NE. 4 )                 ERROR STOP 13
  IF(ANY(  L2 .NEQV. LEndOfRecord  ))   ERROR STOP 14
  IF( KIND(L4) .NE. 4 )                 ERROR STOP 15
  IF(ANY(  L4 .NEQV. LEndOfRecord  ))   ERROR STOP 16
  IF( KIND(L8) .NE. 4 )                 ERROR STOP 17
  IF(ANY(  L8 .NEQV. LEndOfRecord  ))   ERROR STOP 18


  END

