!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 30, 2006
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
!*  a reference to an tranformational intrinsic
!*
!*  - TRANSPOSE
!*  (319498)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    INTEGER    :: I
    CHARACTER  :: C
    LOGICAL(2) :: L(3,3)=.FALSE.
    PROCEDURE(), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM   InitExpDefTRANSPOSE
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE(DT),  PARAMETER :: S(1,128)=RESHAPE((/(DT(ProcPtr=NULL(), L=.TRUE., I=I, C=CHAR(I)), I=1,128)/),(/1,128/))
  INTEGER,   PARAMETER :: II(128) = (/(I, I=1,128)/)
  CHARACTER, PARAMETER :: CC(128) = (/(CHAR(I), I=1,128)/)

  TYPE(DT),  PARAMETER :: T1(128,1) = TRANSPOSE(MATRIX=S)
  INTEGER,   PARAMETER :: T2(128,1) = TRANSPOSE(S(:,:)%I)
  CHARACTER, PARAMETER :: T3(128,1) = TRANSPOSE(S%C)
  INTEGER,   PARAMETER :: T4(1,128)   = TRANSPOSE(TRANSPOSE(S(1:,:)%I))
  CHARACTER, PARAMETER :: T5(1,128)   = TRANSPOSE(TRANSPOSE(S%C))
  TYPE(DT),  PARAMETER :: T6(1,128)   = TRANSPOSE(TRANSPOSE(S(:,:)))


  IF ( ANY( T1(:,1)%I .NE. II)) ERROR STOP 11
  IF ( ANY( T1(:,1)%C .NE. CC)) ERROR STOP 12

  IF ( ANY( T2(:,1)   .NE. II)) ERROR STOP 13
  IF ( ANY( T3(:,1)   .NE. CC)) ERROR STOP 14

  IF ( ANY( T4(:,:)   .NE. RESHAPE(II, (/1,128/)) )) ERROR STOP 15
  IF ( ANY( T5(:,:)   .NE. RESHAPE(CC, (/1,128/)) )) ERROR STOP 16

  IF ( ANY( T6(:,:)%I .NE. RESHAPE(II, (/1,128/)) )) ERROR STOP 17
  IF ( ANY( T6(:,:)%C .NE. RESHAPE(CC, (/1,128/)) )) ERROR STOP 18

  END



