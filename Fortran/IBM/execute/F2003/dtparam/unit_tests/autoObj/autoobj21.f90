!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj21
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
!*  The length parameter depends on a V in host 
!*
!*  ()
!*
!234567892123456789212345678921234567892123456789212345678921234567890

  PROGRAM autoobj21
  INTEGER N
  N = 10

  CALL sub()
  CONTAINS

  SUBROUTINE Sub()

  TYPE dt(l)
     INTEGER, LEN :: l
     INTEGER      :: arr(l)=-1
  END TYPE 

  TYPE(dt(N*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr
 
  IF (b%l         .NE. 20)  STOP 21
  IF (SIZE(b%arr) .NE. 20)  STOP 12
  IF (ANY(b%arr   .NE. -1)) STOP 13

  END SUBROUTINE
  END

