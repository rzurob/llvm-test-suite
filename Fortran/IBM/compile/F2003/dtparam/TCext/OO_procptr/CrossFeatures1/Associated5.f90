! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures1/Associated5.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Associated5.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Associated5.f
!*
!*  DATE                       : May. 9, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  Procedure pointer with data target.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 0
    END TYPE
  END MODULE

  PROGRAM Associated5
  USE M
  IMPLICIT NONE

  PROCEDURE(TYPE(DT(20,4))), POINTER :: ProcPtr=>NULL()
  TYPE ( DT(20,4) ),   TARGET        :: V

  TYPE(DT(:,4)), POINTER    :: DataPtr=>NULL()
  PROCEDURE (TYPE(DT(20,4))) :: Fun

  PROCEDURE(TYPE(DT(20,4))), POINTER :: ProcPtr1=>NULL()
  TYPE(DT(:,4)),            POINTER :: DataPtr1=>NULL()



  ProcPtr => V

  DataPtr => Fun

  ProcPtr1 => DataPtr1

  DataPtr1 => ProcPtr1

  END

  FUNCTION Fun()
  USE M
  TYPE (DT(20,4)) :: Fun
    Fun = DT(20,4)(-1)
  END FUNCTION

