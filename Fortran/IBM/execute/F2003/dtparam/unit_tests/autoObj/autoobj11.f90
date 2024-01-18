!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj11
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 30, 2008
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : Feature Number 333321 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*
!*  The length parameter depends on a V in common block 
!*
!*  ()
!*
!234567891123456789112345678911234567891123456789112345678911234567890

  PROGRAM autoobj11
  INTEGER :: N
  COMMON N
  N = 10
  CALL sub()
  END

  SUBROUTINE Sub()

  INTEGER :: N
  COMMON N

  TYPE dt(l)
     INTEGER, LEN :: l
     INTEGER      :: arr(l)=-1
  END TYPE 

  TYPE(dt(N*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr
 
  IF (b%l         .NE. 20)  STOP 11
  IF (SIZE(b%arr) .NE. 20)  STOP 12
  IF (ANY(b%arr   .NE. -1)) STOP 13

  END SUBROUTINE

