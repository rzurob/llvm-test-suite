!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj88
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
!*  emtry base derived types 
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM autoobj88

  CALL sub(4)
  CONTAINS

  SUBROUTINE Sub(N)

  TYPE base(l)
    INTEGER, LEN :: l
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(1:l)
  END TYPE

  TYPE(dt(n)) t!(n)

    IF (t%arr%l          .NE. 4)   STOP 11
    IF (UBOUND(t%arr, 1) .NE. 4)   STOP 12
    IF (SIZE(t%arr)      .NE. 4)   STOP 13

  END SUBROUTINE

  END

