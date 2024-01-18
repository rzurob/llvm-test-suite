!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemIS_IOSTAT_END.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 11, 2006
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
!*  a reference to an elemental intrinsic
!* 
!*  -  IS_IOSTAT_END 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemIS_IOSTAT_END
  IMPLICIT  NONE
  INTEGER :: I, J

  INTEGER(1), PARAMETER :: IEndOfFile1(4)=(/1,0,-1,-2/)
  INTEGER(2), PARAMETER :: IEndOfFile2(4)=(/1,0,-1,-2/)
  INTEGER(4), PARAMETER :: IEndOfFile4(4)=(/1,0,-1,-2/)
  INTEGER(8), PARAMETER :: IEndOfFile8(4)=(/1,0,-1,-2/)
  LOGICAL,    PARAMETER :: LEndOfFile(4)=(/.FALSE.,.FALSE.,.TRUE.,.TRUE./)

  LOGICAL(KIND(IS_IOSTAT_END(IEndOfFile1) )), PARAMETER :: L1(4)=IS_IOSTAT_END(I=IEndOfFile1) 
  LOGICAL(KIND(IS_IOSTAT_END(IEndOfFile2) )), PARAMETER :: L2(4)=IS_IOSTAT_END(IEndOfFile2) 
  LOGICAL(KIND(IS_IOSTAT_END(IEndOfFile4) )), PARAMETER :: L4(4)=IS_IOSTAT_END(i=IEndOfFile4) 
  LOGICAL(KIND(IS_IOSTAT_END(IEndOfFile8) )), PARAMETER :: L8(4)=IS_IOSTAT_END(IEndOfFile8) 

  LOGICAL, PARAMETER :: L0(1:0)=IS_IOSTAT_END(IEndOfFile8(1:0)) 


  IF( KIND(L1) .NE. 4 )               STOP 11
  IF(ANY(  L1 .NEQV. LEndOfFile  ))   STOP 12
  IF( KIND(L2) .NE. 4 )               STOP 13
  IF(ANY(  L2 .NEQV. LEndOfFile  ))   STOP 14
  IF( KIND(L4) .NE. 4 )               STOP 15
  IF(ANY(  L4 .NEQV. LEndOfFile  ))   STOP 16
  IF( KIND(L8) .NE. 4 )               STOP 17
  IF(ANY(  L8 .NEQV. LEndOfFile  ))   STOP 18


  END


