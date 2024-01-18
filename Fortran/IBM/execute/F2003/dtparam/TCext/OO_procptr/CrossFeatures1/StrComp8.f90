! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/StrComp8.f
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: StrComp8.f 
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
!*  TEST CASE NAME             : StrComp8.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 18, 2005
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
!*  Procedure pointer components 
!*  (FaileD - 267618) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
      PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE

    INTERFACE Base
      FUNCTION ExtFun(C, ProcPtr) 
        IMPORT
        TYPE(Base(1,3))                    :: ExtFun
        CHARACTER(3)                  :: C
        PROCEDURE(CHARACTER(3))       :: Proc 
      END FUNCTION
    END INTERFACE

  CONTAINS

  FUNCTION CFun()
  CHARACTER(3) :: CFun
    CFun = "OK!"
  END FUNCTION
 
  END MODULE
 
  FUNCTION ExtFun(C, Proc)
  USE M, ONLY: Base

  TYPE(Base(1,3))                 :: ExtFun
  CHARACTER(3)               :: C
  PROCEDURE(CHARACTER(3))    :: Proc
    ExtFun%ProcPtr => Proc 
    ExtFun%C = C 
  END FUNCTION

  PROGRAM StrComp8  
  USE M, ONLY: Base, CFun
  IMPLICIT CHARACTER(3)(P) 

  TYPE(Base(1,3))  :: U

  U = Base(1,3)("123", NULL())
  IF ( U%C .NE. "123"  )  STOP 21
  IF ( ASSOCIATED(U%ProcPtr))  STOP 22

  U = Base(1,3)("321", CFun)   ! reference to the interface Base(1,3)
  IF ( U%C .NE. "321"  )  STOP 31
  IF ( U%ProcPtr("321") .NE. "OK!" )  STOP 32

  END

