!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj15
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
!*  The length parameter depends on V in common block and call to an entry
!*
!*  ()
!*
!234567891523456789022345678902234567890223456789022345678902234567890

  PROGRAM autoobj15
  INTEGER N
  COMMON  N

  N = 2
  CALL sub1()
  END

  SUBROUTINE Sub()
  INTEGER N
  COMMON  N

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
print*, b 
  IF (b%l            .NE. 2)  STOP 11
  IF (b(1)%arr%l        .NE. 2)  STOP 12
  IF (len(b(1)%arr%c)    .NE. 2)  STOP 13
  IF (SIZE(b(1)%arr) .NE. 2)  STOP 14
  IF (SIZE(b)        .NE. 2)  STOP 15

  do i = 1, n
    do j = lbound(b(i)%arr,1), ubound(b(i)%arr,1)
        b(i)%arr(j)%c = '123'
    end do
  end do

  IF (ANY(b(1)%arr%c  .NE. "12")) STOP 16
  IF (ANY(b(n)%arr%c  .NE. "12")) STOP 17

  END SUBROUTINE

