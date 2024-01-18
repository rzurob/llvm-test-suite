!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj32
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
!*  The length parameter depends on a V through use association 
!*  the component is character
!*
!*  ()
!*
!234567893223456789322345678932234567893223456789322345678932234567890

  MODULE M
  INTEGER :: N=2
  END MODULE

  PROGRAM autoobj32
  USE M

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
  IF (LEN(b%arr)  .NE. 4)  STOP 12
  IF (SIZE(b%arr) .NE. 4)  STOP 13

  ! The .NE. seems wrong! 
  IF (ANY(b%arr   .NE. "1234")) STOP 14

  END SUBROUTINE
  END

