! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltDbl.f  
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
!*  TEST CASE NAME             : SltDbl
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
!*   The selector is of double precision 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltDbl
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  DOUBLE PRECISION,  TARGET :: D(4) = (/1.,2.,3.,4./)

  Ptr => D 

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT 
    STOP 30
  TYPE IS (REAL(4) )
    STOP 34 
  TYPE IS (REAL(8))
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   STOP 31
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 32
    IF ( ANY(As        .NE. (/1._4,2._4,3._4,4._4/)) )  STOP 32
  END SELECT
 
  SELECT TYPE ( Ptr )
  CLASS DEFAULT 
    STOP 40
  TYPE IS (REAL  )
    STOP 41
  TYPE IS (DOUBLE PRECISION)
    IF ( ANY(SHAPE(Ptr) .NE. (/4/))  )   STOP 42
    IF ( LBOUND(Ptr, 1) .NE. 1       )   STOP 43
    IF ( ANY(Ptr        .NE. (/1._8,2._8,3._8,4._8/)) )  STOP 44
  TYPE IS (REAL(16) )
    STOP 45 
  END SELECT
  
  SELECT TYPE ( As => Ptr(::3) )
  TYPE IS (REAL(4))
    STOP 54 
  CLASS DEFAULT 
    STOP 50
  TYPE IS (REAL( 8) )
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   STOP 51
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 52
    IF ( ANY(As        .NE. (/1.,4./)) )  STOP 53
  END SELECT
  
  SELECT TYPE ( As => Ptr(3) )
  CLASS DEFAULT 
    STOP 60
  TYPE IS (DOUBLE PRECISION)
    IF ( As .NE. 3. )  STOP 62
  TYPE IS (REAL(16))
    STOP 64 
  END SELECT
  
  
  END

