! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp /tstdev/OO_poly/selectType/CrossFeatures/Target2.f
! opt variations: -qck -qnok -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Target2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Target2
!*
!*  DATE                       : Feb. 02, 2005
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
!*  Target
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,513)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C0="0"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1(K2,N2)    ! (4,513,4,1025)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      CHARACTER(N2) :: C1="1"
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(K3,N3)    ! (4,513,4,1025,4,2049)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      CHARACTER(N3) :: C2="2"
    END TYPE

    CONTAINS

    FUNCTION GetObj(Arg)
    CLASS(*),TARGET, INTENT(IN) :: Arg
    CLASS(*), POINTER  :: GetObj
      GetObj => Arg
    END FUNCTION

  END MODULE

  PROGRAM Target2
  USE M
  IMPLICIT NONE
  TYPE(DT(4,513,4,1025,4,2049)), TARGET :: V(4,4)

  CALL Sub(V(1:3, 2:4))

  ASSOCIATE( V => V(1:3, 2:4) )
  ASSOCIATE( V => V(1:2,2:3) )
    IF (TRIM(V(1,1)%C2) .NE. "-2") STOP 21
    IF (TRIM(V(2,1)%C1) .NE. "-1") STOP 21
    IF (TRIM(V(2,2)%C0) .NE. "-0") STOP 21
  END ASSOCIATE
  END ASSOCIATE


  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT(4,*,4,*,4,*)), TARGET   :: U(:,:)
  CLASS(DT(4,:,4,:,4,:)), POINTER  :: PU(:,:)
  CHARACTER(2049), POINTER :: P2(:,:)
  CHARACTER(1025), POINTER :: P1(:,:)
  CHARACTER(513),  POINTER :: P0(:,:)
  INTEGER :: i

  IF (ANY(SHAPE(U) .NE. (/3,3/))) STOP 30
  PU => U(1:2,2:3)
  SELECT TYPE( U => U(1:2,2:3) )
  CLASS IS (DT(4,*,4,*,4,*))

    IF (TRIM(U(1,1)%C0) .NE. "0") STOP 31
    IF (TRIM(U(2,1)%C1) .NE. "1") STOP 32
    IF (TRIM(U(2,2)%C2) .NE. "2") STOP 33
    P2 => U%C2
    P2 = "-2"
    P1 => U%DT1%C1
    P1 = "-1"
    P0 => U%DT1%DT0%C0
    P0 = "-0"

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



