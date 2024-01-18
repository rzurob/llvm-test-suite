! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qnodefaultpv /tstdev/OO_poly/selectType/CrossFeatures/SltSequence1.f
! opt variations: -qnock -qnok -ql -qdefaultpv -qreuse=self

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
! %POSTCMD: tcomp SltSequence1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltSequence1
!*
!*  DATE                       : Jan. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Diagnostic : the selector is a var of sequence type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0(K1,K2,N1)    ! (4,1,1025)
      INTEGER, KIND             :: K1,K2
      INTEGER, LEN              :: N1
      SEQUENCE
      INTEGER(K1)               :: IArr(2)=-1
      CHARACTER(kind=K2,len=N1) :: CArr(2)="!"
    END TYPE

    TYPE :: DT1(K3,K4,N2)    ! (4,1,1025)
      INTEGER, KIND       :: K3,K4
      INTEGER, LEN        :: N2
      TYPE(DT0(K3,K4,N2)) :: Seq
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (4,1,1025)
    END TYPE

  END MODULE

  PROGRAM SltSequence1
  USE M
  IMPLICIT NONE
  TYPE (DT0(4,1,1025))  :: U(2,2,2)

S1: SELECT TYPE (S2 => U)
    TYPE IS (DT0(4,1,*))
    END SELECT S1

  END



