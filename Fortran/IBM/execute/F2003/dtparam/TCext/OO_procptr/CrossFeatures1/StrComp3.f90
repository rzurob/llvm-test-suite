! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self /tstdev/OO_procptr/CrossFeatures1/StrComp3.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: StrComp3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp3.f
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
!*  Structure component - initialization-expr
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
!     SEQUENCE  !?
      INTEGER(K1)   :: BaseID=1
      PROCEDURE(LOGICAL(2)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT(K2)    ! (4)
      INTEGER, KIND  :: K2
!     SEQUENCE
      INTEGER(K2)    :: ChildID=2
      TYPE(Base(K2)) :: BComp=Base(K2)(-1, NULL())
    END TYPE

  END MODULE


  PROGRAM StrComp3
  USE M
  IMPLICIT NONE

  TYPE(DT(4)) :: U
  TYPE(DT(4)) :: V=DT(4)(-2,     &
                & Base(4)(-1, NULL()))


  IF ( V%ChildId .NE. -2 ) STOP 12
  IF ( V%BComp%BaseId .NE. -1 ) STOP 11
  IF ( ASSOCIATED(V%BComp%ProcPtr) ) STOP 13

  IF ( U%ChildId .NE. 2 ) STOP 22
  IF ( U%BComp%BaseId .NE. -1 ) STOP 21
  IF ( ASSOCIATED(U%BComp%ProcPtr) ) STOP 23


  END

