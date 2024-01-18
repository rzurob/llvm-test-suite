!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj25
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
!*  The length parameter depends on V in host,  and call an entry
!*
!*  ()
!*
!234567892523456789022345678902234567890223456789022345678902234567890

  MODULE M
  INTEGER N

  CONTAINS

  SUBROUTINE Sub()

  TYPE base(l) 
    INTEGER, LEN :: l
    CHARACTER(l) :: c="123"
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l)
  END TYPE 

  TYPE(dt(n)) b(n)

  ENTRY Sub1()

print*, b%l
print*, SIZE(b(1)%arr)
print*, len(b(1)%arr%c)
  IF (b%l            .NE. 2)  STOP 11
  IF (b(1)%arr%l        .NE. 2)  STOP 12
  IF (len(b(1)%arr%c)    .NE. 2)  STOP 13
  IF (SIZE(b(1)%arr) .NE. 2)  STOP 14
  IF (SIZE(b)        .NE. 2)  STOP 25

  b(1)%arr%c = '345'
  b(n)%arr%c = '222'

  IF (ANY(b(1)%arr%c  .NE. "34")) STOP 16
  IF (ANY(b(n)%arr%c  .NE. "22")) STOP 17

  END SUBROUTINE
  END MODULE

  PROGRAM autoobj25
  USE M
  N = 2
  CALL sub1()

  END

