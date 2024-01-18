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
!*  - TRIM
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER(0)  :: C0
    CHARACTER     :: C
    CHARACTER(8)  :: CC
    CHARACTER(8)  :: CCC(8)
    PROCEDURE(CHARACTER), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM  InitExpDefTRIM
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE(DT), PARAMETER :: S(8)=                                       &
                    RESHAPE((/(DT(                                   &
                                  ProcPtr=NULL(),                    &
                                  C0="",                             &
                                  C=CHAR(I), CC=CHAR(I)//" ",        &
                                  CCC=(/(CHAR(J)//"  ", J=1,8)/)),   &
                    I=1,8)/),(/8/))

  INTEGER,   PARAMETER :: T1 = LEN(TRIM(S(1)%C0))
  CHARACTER, PARAMETER :: T2 = TRIM(S(1)%CC)
  CHARACTER, PARAMETER :: T3 = TRIM(S(8)%CCC(8))


  IF ( T1 .NE. 0 )          STOP 11
  IF ( T2 .NE. CHAR(1) )    STOP 12
  IF ( T3 .NE. CHAR(8) )    STOP 13


  END



