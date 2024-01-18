! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/initExp/Def/InitExpDefElemAINT.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemAINT.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 23, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
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

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    REAL(NINT(AINT(4.1)))  :: R(INT(AINT(3.1)),INT(AINT(3.1)))=AINT(IC)
  END TYPE

  REAL(NINT(AINT(8.1))) :: T1(NINT(AINT(3.1)),NINT(AINT(3.1))) = AINT(IC1)

  TYPE(DT(4,20)), PARAMETER :: T2(INT(AINT(3.1)):INT(AINT(5.1)), NINT(AINT(3.1)):NINT(AINT(5.1))) &
                        = DT(4,20)(AINT(IC1(:,:)))

  TYPE, EXTENDS(DT) :: DT1    ! (4,20)
  END TYPE

  TYPE(DT1(4,20)) :: T3=DT1(4,20)(AINT(T2(3:,3:)%R(1,1)))  

   
  IF (ANY(LBOUND(IC)   .NE. (/1, 1/)) )             STOP 11
  IF (ANY(UBOUND(IC)   .NE. (/3, 3/)) )             STOP 12
  IF (ANY(IC           .NE.  -1.5   ) )             STOP 13

  IF (ANY(LBOUND(IC1)  .NE. (/1, 1/)) )             STOP 21
  IF (ANY(UBOUND(IC1)  .NE. (/3, 3/)) )             STOP 22
  IF (ANY(IC1          .NE.  -2.5   ) )             STOP 23

  IF (ANY(LBOUND(T1)   .NE. (/1, 1/)) )             STOP 31
  IF (ANY(UBOUND(T1)   .NE. (/3, 3/)) )             STOP 32
  IF (ANY(T1           .NE.  -2     ) )             STOP 33

  IF (ANY(LBOUND(T2)   .NE. (/3, 3/)) )             STOP 41
  IF (ANY(UBOUND(T2)   .NE. (/5, 5/)) )             STOP 42
  DO I=3,5
  DO J=3,5
    IF (ANY(T2(I,J)%R   .NE.  -2     ) )            STOP 43
  END DO
  END DO

  IF (ANY(LBOUND(T3%R)   .NE. (/1, 1/)) )           STOP 51
  IF (ANY(UBOUND(T3%R)   .NE. (/3, 3/)) )           STOP 52
  IF (ANY(T3%R           .NE.  -2     ) )           STOP 53

  END

 
