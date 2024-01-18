! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/StrComp7.f
! opt variations: -qnock -qnok -ql -qdefaultpv

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: StrComp7.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp7.f
!*
!*  DATE                       : May. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Procedure pointer components
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
      PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE

    TYPE  :: DT(K2,K3,N2)    ! (4,1,3)
      INTEGER, KIND     :: K2,K3
      INTEGER, LEN      :: N2
      TYPE(Base(K3,N2)) :: BComp
      PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CHARACTER(3) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  PROGRAM StrComp7
  USE M, B=>Base, D=>DT, IntF=>ModFun
  IMPLICIT CHARACTER(3)(P)

  PROCEDURE(IntF),  POINTER :: ProcPtr=>NULL()
  TYPE(D(4,1,3))  :: U

  ProcPtr => IntF
  IF ( ProcPtr("123") .NE. "123" ) STOP 11

  U = D(4,1,3)(B(1,3)("123", IntF), ProcPtr)
  IF ( U%BComp%C .NE. "123" )              STOP 21
  IF ( .NOT. ASSOCIATED(U%ProcPtr, IntF))  STOP 22
  IF ( U%ProcPtr("!!!") .NE. "!!!" )       STOP 24
  IF ( U%BComp%ProcPtr("!!!") .NE. "!!!" ) STOP 25


  END

