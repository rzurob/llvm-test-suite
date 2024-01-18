!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefStructConstr.f
!*
!*  DATE                       : Mar 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a structure constructor in which each expression corresponding to an allocatable component
!*  is a reference to the intrinsic function NULL, and all other expressions are
!*  initialization expressions;
!*
!*  (318848)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  INTEGER :: IArr(10)

  TYPE :: DT
    INTEGER,    ALLOCATABLE  :: I
    CHARACTER,  ALLOCATABLE  :: C
    REAL ,      ALLOCATABLE  :: R
    PROCEDURE(),NOPASS, POINTER :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE, NOPASS :: ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub()
  END SUBROUTINE

  END MODULE


  PROGRAM InitExpDefStructConstr
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT1
    TYPE(DT) :: Arr(SIZE(IArr), SIZE(IArr))=DT(R=NULL(),C=NULL(),I=NULL())
  END TYPE

  TYPE(DT1) ::  T1,Arr1(SIZE(IArr))
  PARAMETER  (  Arr1=(/(DT1(DT(R=NULL(),C=NULL(),I=NULL())),&
                       I=KIND(1_1), SIZE(IArr))/)  )

  TYPE, EXTENDS(DT) :: DT2
  END TYPE

  TYPE(DT2) :: Arr2(SIZE(IArr))=DT2(DT=DT(R=NULL(),C=NULL(),I=NULL()))

  DO I=1, 10
  DO J=1, 10
    IF (ALLOCATED(T1%Arr(I,J)%I))              STOP 11
    IF (ALLOCATED(T1%Arr(I,J)%C))              STOP 12
    IF (ALLOCATED(T1%Arr(I,J)%R))              STOP 13
    IF (ASSOCIATED(T1%Arr(I,J)%PRocPtr))       STOP 14
  END DO
  END DO

  DO I=1, 10
    IF (ANY(LBOUND(Arr1(I)%Arr)   .NE. (/1, 1 /)) )   STOP 21
    IF (ANY(UBOUND(Arr1(I)%Arr)   .NE. (/10,10/)) )   STOP 22
    DO J=1, 10
    DO K=1, 10
      IF (ALLOCATED(Arr1(I)%Arr(J,K)%I) ) STOP 23
      IF (ALLOCATED(Arr1(I)%Arr(J,K)%C) ) STOP 24
      IF (ALLOCATED(Arr1(I)%Arr(J,K)%R) ) STOP 25
    END DO
    END DO
  END DO


  DO I=1, 10
    IF (ALLOCATED(Arr2(I)%I) )                      STOP 33
    IF (ALLOCATED(Arr2(I)%C) )                      STOP 34
    IF (ALLOCATED(Arr2(I)%R) )                      STOP 35
  END DO

  END


