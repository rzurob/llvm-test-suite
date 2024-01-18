!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj81 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 31, 2009
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
!*  Derived type component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj81
  CALL sub(4)
  contains

  SUBROUTINE Sub(N)

  TYPE base(l)
    INTEGER, LEN :: l
    integer :: i=-1
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr1(1:l,l)
    TYPE(base(l)) :: arr(1:l)
  END TYPE


  TYPE(dt(n)) b!(n)


  IF (b%l         .NE. 4)   STOP 11
  IF (SIZE(b%arr) .NE. 4)   STOP 12
  IF (ANY(b%arr%i .NE. -1)) STOP 13


  END SUBROUTINE

  end
