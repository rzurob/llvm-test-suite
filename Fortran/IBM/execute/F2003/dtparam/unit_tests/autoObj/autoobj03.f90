!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj03
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 25, 2008
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
!*  The length parameter depends on dummy 
!*  the component is another derived type 
!*
!*  ()
!*
!234567890323456789032345678903234567890323456789032345678903234567890

  PROGRAM autoobj03
  CALL sub(2)
  END

  SUBROUTINE Sub(N)

  TYPE base(l) 
    INTEGER, LEN :: l
    CHARACTER(l) :: c="12345"
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l:l+l-1)
  END TYPE 

  TYPE(dt(n*2)) b

print*, b%l
print*, SIZE(b%arr)
print*, ubound(b%arr)
print*, len(b%arr%c)
 
  IF (b%l         .NE. 4)  STOP 11
  IF (b%arr%l     .NE. 4)  STOP 12
  IF (SIZE(b%arr) .NE. 4)  STOP 13
  IF (lbound(b%arr,1) .NE. 4)  STOP 14
  IF (ubound(b%arr,1) .NE. 7)  STOP 15
  IF (len(b%arr(1)%c) .NE. 4)  STOP 16

  IF (ANY(b%arr%c  .NE. "1234")) STOP 17

  END SUBROUTINE

