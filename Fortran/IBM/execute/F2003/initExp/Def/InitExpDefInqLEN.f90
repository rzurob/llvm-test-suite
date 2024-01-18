!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqLEN.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 03, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  a reference to a specification inquiry 
!* 
!*  - LEN 
!* 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefInqLEN 
  IMPLICIT NONE
  INTEGER :: I, J, K

  CHARACTER(0),    PARAMETER :: C0(-2147483648:-2147483647, 2147483646:2147483647) = CHAR(0) 
  CHARACTER(1),    PARAMETER :: C1 = CHAR(127) 
  CHARACTER(9),    PARAMETER :: C9(-2147483648:-2147483647, 2147483646:2147483647) = CHAR(1) 
  CHARACTER(513),  PARAMETER :: C513 = REPEAT("!", 513) 

  INTEGER  :: KTP4 = KIND(LEN(C0))
  INTEGER  :: KTP1 = KIND(LEN(C0, KIND=1))
  INTEGER  :: KTP2 = KIND(LEN(C513, KIND=2))
  INTEGER  :: KTP8 = KIND(LEN(KIND=8, STRING=C9))

  INTEGER  :: TC0 = LEN(C0)
  INTEGER  :: TC1 = LEN(C1)
  INTEGER  :: TC9 = LEN(C9)
  INTEGER  :: TC513 = LEN(C513)

  IF ( KTP4    .NE. 4     )            STOP 11
  IF ( KTP1    .NE. 1     )            STOP 12
  IF ( KTP2    .NE. 2     )            STOP 13
  IF ( KTP8    .NE. 8     )            STOP 14

  IF ( TC0     .NE. 0     )            STOP 21
  IF ( TC1     .NE. 1     )            STOP 22
  IF ( TC9     .NE. 9     )            STOP 23
  IF ( TC513   .NE. 513   )            STOP 24

  END


 
