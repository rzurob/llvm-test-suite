!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqNEW_LINE.f  
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
!*  - NEW_LINE 
!* 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefInqNEW_LINE 
  IMPLICIT NONE
  INTEGER :: I, J, K


  CHARACTER(0),    PARAMETER :: C0(-2147483648:-2147483647, 2147483646:2147483647) = CHAR(0)
  CHARACTER(1),    PARAMETER :: C1 = CHAR(10)
  CHARACTER(9),    PARAMETER :: C9(-2147483648:-2147483647, 2147483646:2147483647) = CHAR(1)
  CHARACTER(513),  PARAMETER :: C513 = REPEAT(CHAR(10), 513)

  CHARACTER  :: C0Char  = NEW_LINE(C0)
  INTEGER    :: C0Kind  = KIND(NEW_LINE(C0))
  CHARACTER  :: C1Char  = NEW_LINE(C1)
  INTEGER    :: C1Kind  = KIND(NEW_LINE(C1))
  CHARACTER  :: C9Char  = NEW_LINE(C9)
  INTEGER    :: C9Kind  = KIND(NEW_LINE(C9))
  CHARACTER  :: C513Char  = NEW_LINE(C513)
  INTEGER    :: C513Kind  = KIND(NEW_LINE(C513))

  CHARACTER  :: CAChar  = NEW_LINE((/ACHAR(10)/))
  INTEGER    :: CAKind  = KIND(NEW_LINE((/ACHAR(10)/)))

  CHARACTER  :: CBChar  = NEW_LINE(ACHAR(10))
  INTEGER    :: CBKind  = KIND(NEW_LINE(ACHAR(10)))

  IF ( C0Char    .NE. ACHAR(10) )   STOP 11
  IF ( C0Kind    .NE. 1     )       STOP 12
  IF ( C1Char    .NE. ACHAR(10) )   STOP 13
  IF ( C1Kind    .NE. 1     )       STOP 14
  IF ( C9Char    .NE. ACHAR(10) )   STOP 15
  IF ( C9Kind    .NE. 1     )       STOP 16
  IF ( C513Char  .NE. ACHAR(10) )   STOP 17
  IF ( C513Kind  .NE. 1     )       STOP 18

  IF ( CAChar    .NE. ACHAR(10) )   STOP 21
  IF ( CAKind    .NE. 1     )       STOP 22

  IF ( CBChar    .NE. ACHAR(10) )   STOP 31
  IF ( CBKind    .NE. 1     )       STOP 32

  END


 
