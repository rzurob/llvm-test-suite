!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 23, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -AINT
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemAINT
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(INT(AINT(4.1))), PARAMETER :: IC(INT(AINT(3.1)),NINT(AINT(3.1)))= -1.5
  REAL(INT(AINT(4.1))), PARAMETER :: IC1(INT(AINT(3.1)),NINT(AINT(3.1)))= -2.5

  TYPE :: DT
    REAL(NINT(AINT(4.1)))  :: R(INT(AINT(3.1)),INT(AINT(3.1)))=AINT(IC)
  END TYPE

  REAL(NINT(AINT(8.1))) :: T1(NINT(AINT(3.1)),NINT(AINT(3.1))) = AINT(IC1)

  TYPE(DT), PARAMETER :: T2(INT(AINT(3.1)):INT(AINT(5.1)), NINT(AINT(3.1)):NINT(AINT(5.1))) &
                        = DT(AINT(IC1(:,:)))

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE(DT1) :: T3=DT1(AINT(T2(3:,3:)%R(1,1)))


  IF (ANY(LBOUND(IC)   .NE. (/1, 1/)) )             ERROR STOP 11
  IF (ANY(UBOUND(IC)   .NE. (/3, 3/)) )             ERROR STOP 12
  IF (ANY(IC           .NE.  -1.5   ) )             ERROR STOP 13

  IF (ANY(LBOUND(IC1)  .NE. (/1, 1/)) )             ERROR STOP 21
  IF (ANY(UBOUND(IC1)  .NE. (/3, 3/)) )             ERROR STOP 22
  IF (ANY(IC1          .NE.  -2.5   ) )             ERROR STOP 23

  IF (ANY(LBOUND(T1)   .NE. (/1, 1/)) )             ERROR STOP 31
  IF (ANY(UBOUND(T1)   .NE. (/3, 3/)) )             ERROR STOP 32
  IF (ANY(T1           .NE.  -2     ) )             ERROR STOP 33

  IF (ANY(LBOUND(T2)   .NE. (/3, 3/)) )             ERROR STOP 41
  IF (ANY(UBOUND(T2)   .NE. (/5, 5/)) )             ERROR STOP 42
  DO I=3,5
  DO J=3,5
    IF (ANY(T2(I,J)%R   .NE.  -2     ) )            ERROR STOP 43
  END DO
  END DO

  IF (ANY(LBOUND(T3%R)   .NE. (/1, 1/)) )           ERROR STOP 51
  IF (ANY(UBOUND(T3%R)   .NE. (/3, 3/)) )           ERROR STOP 52
  IF (ANY(T3%R           .NE.  -2     ) )           ERROR STOP 53

  END

