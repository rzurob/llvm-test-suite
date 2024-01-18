! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/ProcInterface2.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: ProcInterface2.f 
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
!*  TEST CASE NAME             : ProcInterface 
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
!* Procedure Interface 
!* ()
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M
    TYPE  :: DT0(K1)    ! (8)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: I0=0
      CONTAINS
      PROCEDURE, PASS   :: GetInt
    END TYPE

    TYPE,  EXTENDS(DT0) :: DT1    ! (8)
      INTEGER(K1) :: I1=1
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (8)
      INTEGER(K1) :: I2=2
    END TYPE

    CONTAINS

    FUNCTION GetInt(Arg) 
    CLASS(DT0(8)) :: Arg
    INTEGER(8), ALLOCATABLE :: GetInt
      SELECT TYPE (Arg)
      TYPE IS (DT0(8))
        ALLOCATE(GetInt, SOURCE=Arg%I0)
      TYPE IS (DT1(8))
        ALLOCATE(GetInt, SOURCE=Arg%I1)
      TYPE IS (DT(8))
        ALLOCATE(GetInt, SOURCE=Arg%I2)
      CLASS DEFAULT
        STOP 20
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM ProcInterface2
  USE M
  IMPLICIT CLASS(DT(8))(U)

  CALL Sub(DT(8)())

  CONTAINS

  SUBROUTINE Sub(UArg)

  INTERFACE UFun
    FUNCTION UFun(UArg)
      CLASS(*), POINTER :: UFun
      CLASS(*) :: UArg
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( V =>UFun(UArg) )
  CLASS IS (DT(8))

    IF (V%I0 .NE. 0) STOP 30
    IF (V%I1 .NE. 1) STOP 31
    IF (V%I2 .NE. 2) STOP 32

    IF (V%DT0%GetInt() .NE. 0) STOP 40
    IF (V%DT1%GetInt() .NE. 1) STOP 41
    IF (V%GetInt()     .NE. 2) STOP 42

  CLASS DEFAULT
    STOP 50
  END SELECT

  END SUBROUTINE

  END

  FUNCTION UFun(UArg)
  IMPLICIT NONE
  CLASS(*), POINTER :: UFun
  CLASS(*) :: UArg
    ALLOCATE(UFun, SOURCE=UArg)
  END FUNCTION


