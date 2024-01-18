! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/OO_procptr/CrossFeatures2/StrComp1.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: StrComp1.f 
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
!*  TEST CASE NAME             : StrComp1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 24, 2005
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
!*  Structure component - sequence type 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      SEQUENCE
      PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT(K2,N2)    ! (4,20)
      INTEGER, KIND     :: K2
      INTEGER, LEN      :: N2
      SEQUENCE
      PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
      TYPE(Base(K2,N2)) :: BComp
    END TYPE


    CONTAINS

    FUNCTION ModFun1(Arg)
    REAL(8) :: ModFun1, Arg 
      ModFun1 = Arg
    END FUNCTION
 
    FUNCTION ModFun2(Arg)
    REAL(8) :: ModFun2, Arg 
      ModFun2 = Arg
    END FUNCTION
 
  END MODULE


  SUBROUTINE ExtSub(Arg1, Arg2 )

  TYPE :: Base(K3,N3)    ! (4,20)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
    SEQUENCE
    PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
  END TYPE

  TYPE  :: DT(K4,N4)    ! (4,20)
    INTEGER, KIND     :: K4
    INTEGER, LEN      :: N4
    SEQUENCE
    PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
    TYPE(Base(K4,N4)) :: BComp
  END TYPE

  REAL(8)  :: Arg2 
  TYPE(DT(4,*)) :: Arg1

    IF ( .NOT. ASSOCIATED(Arg1%ProcPtr))       STOP 11
    IF ( .NOT. ASSOCIATED(Arg1%BComp%ProcPtr)) STOP 12
    IF ( Arg1%ProcPtr(Arg2)       .NE. Arg2 )  STOP 13 
    IF ( Arg1%BComp%ProcPtr(Arg2) .NE. Arg2 )  STOP 14 
  
  END SUBROUTINE 


  PROGRAM StrComp1  
  USE M
  IMPLICIT NONE 

  INTERFACE
    SUBROUTINE ExtSub(Arg1, Arg2 )
      IMPORT DT
      REAL(8)  :: Arg2
      TYPE(DT(4,*)) :: Arg1
    END SUBROUTINE 
  END INTERFACE

  TYPE(DT(4,20)) :: V

  V%ProcPtr => ModFun2 
  V%BComp%ProcPtr => ModFun1 

  CALL ExtSub(V, 8.0_8)
  CALL ExtSub(V, -8.0_8)

  END

