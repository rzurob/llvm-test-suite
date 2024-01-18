!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpAssgn2.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 07 2006
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
!*  Init with Null
!*
!* (325078)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0
    INTEGER(1), POINTER :: I1(:)
    INTEGER(2), POINTER :: I2(:)
    INTEGER(4), POINTER :: I4(:)
    INTEGER(8), POINTER :: I8(:)

    REAL(4),  POINTER  :: R4(:)
    REAL(8),  POINTER  :: R8(:)
    REAL(16), POINTER  :: R6(:)

    COMPLEX(4),  POINTER :: Z4(:)
    COMPLEX(8),  POINTER :: Z8(:)
    COMPLEX(16), POINTER :: Z6(:)

    CHARACTER(LEN=1), POINTER :: C(:)
  END TYPE

  END MODULE

  PROGRAM InitExpAssgn2
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT
    TYPE(DT0) :: T1=DT0(      &
                 I1=NULL(),  &
                 I2=NULL(),  &
                 I4=NULL(),  &
                 I8=NULL(),  &
                 R4=NULL(),  &
                 R8=NULL(),  &
                 R6=NULL(),  &
                 Z4=NULL(),  &
                 Z8=NULL(),  &
                 Z6=NULL(),  &
                 C =NULL()   &
                       )
  END TYPE


  TYPE (DT) :: T(128)=[(DT(T1=DT0( &
                 I1=NULL(),  &
                 I2=NULL(),  &
                 I4=NULL(),  &
                 I8=NULL(),  &
                 R4=NULL(),  &
                 R8=NULL(),  &
                 R6=NULL(),  &
                 Z4=NULL(),  &
                 Z8=NULL(),  &
                 Z6=NULL(),  &
                 C =NULL()  )&
                 ), I=0,127)]



  DO I=1, 128
    IF (  ASSOCIATED(T(I)%T1%I1 )) ERROR STOP 11
    IF (  ASSOCIATED(T(I)%T1%I2 )) ERROR STOP 12
    IF (  ASSOCIATED(T(I)%T1%I4 )) ERROR STOP 13
    IF (  ASSOCIATED(T(I)%T1%I8 )) ERROR STOP 14

    IF (  ASSOCIATED(T(I)%T1%R4 )) ERROR STOP 21
    IF (  ASSOCIATED(T(I)%T1%R8 )) ERROR STOP 22
    IF (  ASSOCIATED(T(I)%T1%R6 )) ERROR STOP 23

    IF (  ASSOCIATED(T(I)%T1%Z4 )) ERROR STOP 31
    IF (  ASSOCIATED(T(I)%T1%Z8 )) ERROR STOP 32
    IF (  ASSOCIATED(T(I)%T1%Z6 )) ERROR STOP 33

    IF (  ASSOCIATED(T(I)%T1%C  )) ERROR STOP 41
  END DO


  END



