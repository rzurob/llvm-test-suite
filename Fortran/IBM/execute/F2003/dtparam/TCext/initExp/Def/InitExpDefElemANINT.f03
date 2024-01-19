! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/initExp/Def/InitExpDefElemANINT.f
! opt variations: -qnok -qnol

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
!*  -ANINT
!*  (318902)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemANINT
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(NINT(4.1, 8)), PARAMETER :: IC(NINT(5.1),NINT(5.1))= ANINT(-1.4, 8)
  REAL(NINT(4.1, 8)), PARAMETER :: IC1(NINT(5.1),NINT(5.1))=ANINT(-2.4, 8)

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    REAL(NINT(4.1, 8)) :: R(NINT(3.1),NINT(3.1))=ANINT(IC1(::2,::2)*IC(::2,::2))
  END TYPE

  REAL(NINT(8.1, 8)) :: T1(NINT(3.1),NINT(3.1)) = ANINT(IC1(1:3, ::2))

  TYPE(DT(4,20)), PARAMETER :: T2(NINT(3.1):NINT(5.1), NINT(3.1):NINT(5.1)) &
                        = DT(4,20)(ANINT(IC(1:3, ::2)+IC1(::2, 1:3)))

  TYPE, EXTENDS(DT) :: DT1(K2,N2)    ! (4,20,4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
  END TYPE

  TYPE(DT1(4,20,4,20)) :: T3=DT1(4,20,4,20)(AINT(T2(:,3:)%R(1,1)))


  IF (ANY(LBOUND(IC)   .NE. (/1, 1/)) )             ERROR STOP 11
  IF (ANY(UBOUND(IC)   .NE. (/5, 5/)) )             ERROR STOP 12
  IF (ANY(IC           .NE.  -1.0   ) )             ERROR STOP 13

  IF (ANY(LBOUND(IC1)  .NE. (/1, 1/)) )             ERROR STOP 21
  IF (ANY(UBOUND(IC1)  .NE. (/5, 5/)) )             ERROR STOP 22
  IF (ANY(IC1          .NE.  -2.0   ) )             ERROR STOP 23

  IF (ANY(LBOUND(T1)   .NE. (/1, 1/)) )             ERROR STOP 31
  IF (ANY(UBOUND(T1)   .NE. (/3, 3/)) )             ERROR STOP 32
  IF (ANY(T1           .NE.  -2.0   ) )             ERROR STOP 33

  IF (ANY(LBOUND(T2)   .NE. (/3, 3/)) )             ERROR STOP 41
  IF (ANY(UBOUND(T2)   .NE. (/5, 5/)) )             ERROR STOP 42
  DO I=3,5
  DO J=3,5
    IF (ANY(T2(I,J)%R   .NE.  -3     ) )            ERROR STOP 43
  END DO
  END DO

  IF (ANY(LBOUND(T3%R)   .NE. (/1, 1/)) )           ERROR STOP 51
  IF (ANY(UBOUND(T3%R)   .NE. (/3, 3/)) )           ERROR STOP 52
  IF (ANY(T3%R           .NE.  -3     ) )           ERROR STOP 53

  END


