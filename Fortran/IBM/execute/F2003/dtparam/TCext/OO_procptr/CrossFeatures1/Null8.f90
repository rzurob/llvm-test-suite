! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Null8.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Null8.f 
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
!*  TEST CASE NAME             : Null8.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 11, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
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
!*   null()
!*   MOLD shall also be present if the reference appears as an actual argument
!*   corresponding to a  dummy argument with assumed character length.
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      SUBROUTINE fSub(Arg)
      CHARACTER(*), POINTER :: Arg
      END SUBROUTINE
    END INTERFACE

    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(FSub), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

  CONTAINS

    SUBROUTINE IntSub(Arg)
    CHARACTER(*), POINTER :: Arg

    IF ( ASSOCIATED(Arg)) STOP 11 
    IF (LEN(Arg) .NE. 3 ) STOP 12

    END SUBROUTINE
 
  END MODULE

  PROGRAM Null8 
  USE M
  IMPLICIT NONE 
  TYPE(DT(4)) :: V
  CHARACTER(3), POINTER      :: CPtr=>NULL()
  PROCEDURE(IntSub), POINTER :: ProcPtr
  CHARACTER(3), TARGET       :: CTar="123"

  CALL IntSub(NULL(CPtr))

  ProcPtr => IntSub 
  CALL ProcPtr(NULL(CPtr))

  V%ProcPtr => IntSub 
  CALL V%ProcPtr(NULL(CPtr))

  ProcPtr => IntSub 
  CPtr => CTar
  CALL ProcPtr(NULL(CPtr))

  END



