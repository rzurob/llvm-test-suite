! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Misc15.f 
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
!*  TEST CASE NAME             :  Misc15.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 09, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
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
!*  Invocation of proc ptr 
!*  
!*  
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT INTEGER(P)

  TYPE :: DT
    SEQUENCE
    INTEGER :: ID=1
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  INTEGER :: ModFun, Arg
    ModFun = Arg
  END FUNCTION

  END MODULE

  PROGRAM Misc15
  USE M
  IMPLICIT INTEGER(P)

  PROCEDURE(), POINTER :: ProcPtr
  TYPE(DT)             :: V
  TYPE(DT)             :: U(10000)

  ProcPtr => ModFun

  IF (ProcPtr(-1) .NE. -1 ) STOP 11
  DO I=ProcPtr(10000), ProcPtr(1), ProcPtr(-1)
    IF (ProcPtr(ProcPtr(I)) .NE. I ) STOP 12
    SELECT CASE (ProcPtr(I))
    CASE DEFAULT 
      IF (ProcPtr(I) .NE. I ) STOP 13
    END SELECT
  END DO

  V%ProcPtr => ModFun

  IF (V%ProcPtr(-1) .NE. -1 ) STOP 21
  DO I=V%ProcPtr(10000), V%ProcPtr(1), V%ProcPtr(-1)
    IF (V%ProcPtr(V%ProcPtr(I)) .NE. I ) STOP 22
    SELECT CASE (V%ProcPtr(I))
    CASE DEFAULT 
      IF (V%ProcPtr(I) .NE. I ) STOP 23
    END SELECT

    WHERE ((/(.TRUE., I=1,10000)/) )
      U = DT(-1, ModFun)
    END WHERE
  END DO

  DO I=1,10000
    IF (U(I)%ID .NE. -1 ) STOP 31
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, ModFun) ) STOP 32
  END DO

  END


