! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures1/Misc21.f
! opt variations: -qnok -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Misc21.f
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
!*  TEST CASE NAME             :  Misc21.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 13, 2005
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
!*  Procedure pointer component with implicit interface 
!*  - should only be associated with subroutine
!*  
!*  (Failed: implicit interface problem)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT INTEGER(P) 

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  TYPE :: DT1(K2)    ! (4)
    INTEGER, KIND :: K2
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr
  END TYPE

  LOGICAL L
  PROCEDURE(), POINTER :: ProcPtr
 
  CONTAINS

  FUNCTION F()
  INTEGER F
    L = .TRUE.
    F = 1
  END FUNCTION

  SUBROUTINE S()

    ProcPtr => F  ! Test if affect the definition of proc ptr component
    IF ( ProcPtr() .NE. 1 ) STOP 11

    L = .TRUE.
    PRINT*, "In Sub"
  END SUBROUTINE

  END MODULE

  USE M

  TYPE(DT(4))  :: U(1), V
  TYPE(DT1(4)) :: W

  L = .FALSE.
  V%PRocPtr => S
  CALL V%ProcPtr()
  IF( .NOT. L) STOP 11

  L = .FALSE.
  U(1)%ProcPtr => S
  CALL U(1)%ProcPtr()
  IF( .NOT. L) STOP 12

  L = .FALSE.
  W%ProcPtr => F
  PRINT*, W%ProcPtr()
  IF( .NOT. L) STOP 13

END


