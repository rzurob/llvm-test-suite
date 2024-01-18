! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltByte.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltByte 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 13, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
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
!*   The selector is of byte 
!*    (Comp failed: 296997)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltByte 
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  BYTE, TARGET      :: BTar(4) = (/1, 2, 3, 4/)

  Ptr => BTar

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT 
    STOP 30
  TYPE IS ( BYTE)
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   STOP 31
    IF ( LBOUND(As ,1) .NE. 1       )   STOP 32
    IF ( ANY(As        .NE. (/1,2,3,4/)) )  STOP 32
  TYPE IS (CHARACTER(*) )
    STOP 34 
  END SELECT
  
  SELECT TYPE ( As => Ptr(::3) )
  CLASS DEFAULT 
    STOP 30
  TYPE IS (  BYTE )
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   STOP 31
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 32
    IF ( ANY(As        .NE. (/1,4/)) )  STOP 32
  TYPE IS (CHARACTER(*))
    STOP 34 
  END SELECT
  
  SELECT TYPE ( As => Ptr(3) )
  CLASS DEFAULT 
    STOP 40
  TYPE IS (     BYTE)
    IF ( As   .NE. 3 )  STOP 42
  TYPE IS (CHARACTER(*)            )
    STOP 34 
  END SELECT
  
  
  END

