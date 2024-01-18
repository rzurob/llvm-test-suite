!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj22
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
!*  the component is character
!*
!*  ()
!*
!234567892223456789222345678922234567892223456789222345678922234567890

  PROGRAM autoobj22
  INTEGER N

  N = 2
  CALL sub()

  CONTAINS

  SUBROUTINE Sub()

  TYPE dt(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: arr(l)="12345"
  END TYPE 

  TYPE(dt(n*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr
 
  IF (b%l         .NE. 4)  STOP 11
  IF (LEN(b%arr)  .NE. 4)  STOP 22
  IF (SIZE(b%arr) .NE. 4)  STOP 13

  IF (ANY(b%arr   .NE. "1234")) STOP 14

  END SUBROUTINE
  END

