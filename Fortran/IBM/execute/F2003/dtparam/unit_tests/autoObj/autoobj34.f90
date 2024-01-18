!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj34
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
!*  The length parameter depends on V through use association 
!*  the array bound relies on v 
!*
!*  ()
!*
!234567893423456789022345678902234567890223456789022345678902234567890

  MODULE M
  INTEGER :: N=2
  END MODULE

  PROGRAM autoobj34
  USE M

  CALL sub()

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

print*, b%l
print*, SIZE(b(1)%arr)
print*, len(b(1)%arr%c)
 
  IF (b%l            .NE. 2)  STOP 11
  IF (b(1)%arr%l        .NE. 2)  STOP 12
  IF (len(b(1)%arr%c)    .NE. 2)  STOP 13
  IF (SIZE(b(1)%arr) .NE. 2)  STOP 14
  IF (SIZE(b)        .NE. 2)  STOP 15

  do i = 1, n
    b(i)%arr%c = '2345'
  end do

  IF (ANY(b(1)%arr%c  .NE. "23")) STOP 16
  IF (ANY(b(n)%arr%c  .NE. "23")) STOP 17

  END SUBROUTINE
  END

