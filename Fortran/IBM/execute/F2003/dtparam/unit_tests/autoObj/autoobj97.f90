!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj96
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
!*  Basic on default init on autoobj 
!*  (RTO issue)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj97

  CALL sub1(4)
  CALL sub(4)

! contains
  END

  SUBROUTINE Sub(n)

  TYPE dt(l)
    INTEGER, LEN :: l
    INTEGER :: arr(l)=-1
  END TYPE

  TYPE(dt(n)) b(n)

  ENTRY Sub1(n)


  IF (b%l            .NE. 4)  STOP 11
  IF (SIZE(b(1)%arr) .NE. 4)  STOP 12
  IF (SIZE(b)        .NE. 4)  STOP 13
  IF (ANY(b(1)%arr   .NE. -1 ))  STOP 14

  END SUBROUTINE


