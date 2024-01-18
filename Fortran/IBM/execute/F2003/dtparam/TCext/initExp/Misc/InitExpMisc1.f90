! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv /tstdev/F2003/initExp/Misc/InitExpMisc1.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpMisc1.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 11 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Charber 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Misc  on pack
!*
!* (325095) -- a "dup" one of InitExpMisc.f by removing ac-imp-do -- Why? The ac-imp-do issue will not be fixed soon.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  CONTAINS
  PROCEDURE :: MyAssgn
  GENERIC :: ASSIGNMENT(=) => MyAssgn
  END TYPE

  CONTAINS

  ELEMENTAL SUBROUTINE MyAssgn(Arg1, Arg2)
  CLASS(DT0(*,4)), INTENT(INOUT) :: Arg1
  TYPE(DT0(*,4)), INTENT(IN)    :: Arg2
    Arg1%ID = -Arg2%ID
  END SUBROUTINE

  END MODULE

  PROGRAM InitExpMisc1
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT(K2,N2)    ! (4,20)
    INTEGER, KIND    :: K2
    INTEGER, LEN     :: N2
    TYPE(DT0(N2,K2)) :: T
  END TYPE


  TYPE(DT(4,20)), PARAMETER :: A(9) = [(DT(4,20)(DT0(20,4)(I)),I=1,9)]

  TYPE (DT(4,20)) :: T(9)=[    PACK(A(1:4), .TRUE.),  &
                         A(5),                  &
                         PACK(A(6:9), .TRUE.)   ]

  DO I=1, 9
      IF ( T(I)%T%ID .NE.  A(I)%T%ID )  STOP 11
  END DO

  END



