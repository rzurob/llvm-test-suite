! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qreuse=self /tstdev/OO_poly/selectType/CrossFeatures/SltSequence.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
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
!*  componet of  sequence type
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

    TYPE :: DT1(K3,N2,K4,N3)    ! (4,20,1,1025)
      INTEGER, KIND       :: K3,K4
      INTEGER, LEN        :: N2,N3
      TYPE(DT0(K3,K4,N3)) :: Seq
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (4,20,1,1025)
    END TYPE

  END MODULE

  PROGRAM SltSequence
  USE M
  IMPLICIT NONE
  TYPE(DT(4,20,1,1025)) :: V(2,2,2)

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT1(4,*,1,*))  :: U(:,:,:)

S1: SELECT TYPE (S2 => U)
    CLASS DEFAULT

S2: SELECT TYPE (U => S2 )
    CLASS DEFAULT
      STOP 20
    CLASS IS (DT1(4,*,1,*))

        IF (ANY(U(:,:,:)%Seq%IArr(1)  .NE. -1)) STOP 21
        IF (ANY(U(:,:,:)%Seq%IArr(2)  .NE. -1)) STOP 22
        IF (TRIM(U(1,1,1)%Seq%CArr(1))  .NE. "!") STOP 23
        IF (TRIM(U(1,1,1)%Seq%CArr(2))  .NE. "!") STOP 24

        U%Seq%IArr(1) = 1
        U%Seq%IArr(2) = 2
        U%Seq%CArr(1) = "K"
        U%Seq%CArr(2) = "Q"

        IF (SIZE(U(2,2,2)%Seq%IArr)   .NE. 2)  STOP 30
        IF (KIND(U(2,2,2)%Seq%IArr)   .NE. 4)  STOP 31
        IF (ANY(U(:,:,:)%Seq%IArr(1)  .NE. 1)) STOP 32
        IF (ANY(U(:,:,:)%Seq%IArr(2)  .NE. 2)) STOP 33
        IF (ANY(U(:,:,:)%Seq%CArr(1)  .NE. "K")) STOP 22
        IF (ANY(U(:,:,:)%Seq%CArr(2)  .NE. "Q")) STOP 23

    END SELECT S2
    END SELECT S1

  END SUBROUTINE

  END



