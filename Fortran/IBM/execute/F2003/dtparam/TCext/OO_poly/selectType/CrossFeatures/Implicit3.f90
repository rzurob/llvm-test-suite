! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_poly/selectType/CrossFeatures/Implicit3.f
! opt variations: -qck -qnok

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Implicit3.f 
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
!*  TEST CASE NAME             : Implicit 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2005
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
!* Implicit 
!* ()
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,513)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: SetObj
    END TYPE

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE,  EXTENDS(DT0) :: DT1(N2)    ! (4,513,1025)
      INTEGER, LEN  :: N2
      CHARACTER(N2) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(N3)    ! (4,513,1025,2049)
      INTEGER, LEN  :: N3
      CHARACTER(N3) :: C2="2"
    END TYPE

    TYPE (DT(4,513,1025,2049)), SAVE, TARGET :: V

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0(4,*)) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE 

  END MODULE

  PROGRAM Implicit3 
  USE M
  IMPLICIT CLASS(DT(4,513,1025,2049))(V,U)

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)
 
  SELECT TYPE (V=>U)
  CLASS IS (DT(4,*,*,*))
    IF (TRIM(V%C0) .NE. "0") STOP 20
    IF (TRIM(V%C1) .NE. "1") STOP 21
    IF (TRIM(V%C2) .NE. "2") STOP 22

    V%DT0%C0 ="?"
    V%DT1%C1 ="?"
    V%C2 ="?"
    
  CLASS DEFAULT
    STOP 40
  END SELECT

    IF (TRIM(V%C0) .NE. "?") STOP 30
    IF (TRIM(V%C1) .NE. "?") STOP 31
    IF (TRIM(V%C2) .NE. "?") STOP 32

  END SUBROUTINE

  END



