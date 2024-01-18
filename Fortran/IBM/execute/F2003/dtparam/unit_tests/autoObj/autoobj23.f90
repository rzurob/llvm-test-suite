!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj23
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
!*  the component is another derived type 
!*
!*  ()
!*
!234567892323456789232345678923234567892323456789232345678923234567890

  PROGRAM autoobj23
  INTEGER N

  N = 2
  CALL sub()

  CONTAINS

  SUBROUTINE Sub()

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
print*, len(b%arr%c)
 
  IF (b%l         .NE. 4)  STOP 11
  IF (b%arr%l     .NE. 4)  STOP 12
  IF (SIZE(b%arr) .NE. 4)  STOP 13

  do i = b%l, 2*b%l-1
    b%arr(i)%c = '12345'
  end do
  IF (ANY(b%arr%c  .NE. "1234")) STOP 14

  END SUBROUTINE
  END

