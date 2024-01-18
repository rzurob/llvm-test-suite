! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/F2003/kindArg/kindArg/kindArgIachar8.f
! opt variations: -qnol -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIachar8
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR 
!*
!*  REFERENCE                  : Feature Number 289083 
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
!*   
!*  
!*  An undefined value is returned if C ins not in the ASCII colating sequence.
!*  The results are consistent with the LGE, LGT, LLE, and LLT lexical comparison functions. 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIachar8
  IMPLICIT NONE

  INTEGER(1) :: I1, II1(128), K1
  INTEGER(2) :: I2, II2(128), K2
  INTEGER(4) :: I4, II4(128), K4
  INTEGER(8) :: I8, II8(128), K8

  INTEGER   :: I, J
  LOGICAL   :: L
  CHARACTER :: CC(0:127)=(/(ACHAR(I), I=0,127)/)
  
  TYPE :: DT(N1,D1,D2,D3,D4)    ! (20,1,1,1,1)
    INTEGER, KIND :: D1,D2,D3,D4
    INTEGER, LEN  :: N1
    INTEGER(D1)   :: K1=0
    INTEGER(D2)   :: K2=0
    INTEGER(D3)   :: K4=0
    INTEGER(D4)   :: K8=0
  END TYPE

  TYPE (DT(20,1,1,1,1)), PARAMETER :: T(128)=DT(20,1,1,1,1)(1,2,4,8)


  DO I = 0, 127 
  DO J = I, 127 
    L = LGE(ACHAR(I=J, KIND=KIND(T%K1)),       ACHAR(I=I, KIND=KIND(T%K1))) .AND. &
            (IACHAR(CC(J), KIND=KIND(T%K1)) .GE. IACHAR(CC(I), KIND=KIND(T%K1)))
    IF (.NOT. L) STOP 11
  END DO
  END DO

  DO I = 0, 127 
  DO J = I+1, 127 
    L = LGT(ACHAR(I=J, KIND=KIND(T%K1)),       ACHAR(I=I, KIND=KIND(T%K1))) .AND. &
            (IACHAR(CC(J), KIND=KIND(T%K2)) .GE. IACHAR(CC(I), KIND=KIND(T%K2)))
    IF (.NOT. L) STOP 12
  END DO
  END DO

  DO I = 0, 127 
  DO J = I, 127 
    L = LLE(ACHAR(I=I, KIND=KIND(T%K1)),       ACHAR(I=J, KIND=KIND(T%K1))) .AND. &
            (IACHAR(CC(J), KIND=KIND(T%K4)) .GE. IACHAR(CC(I), KIND=KIND(T%K4)))
    IF (.NOT. L) STOP 11
  END DO
  END DO

  DO I = 0, 127 
  DO J = I+1, 127 
    L = LLT(ACHAR(I=I, KIND=KIND(T%K1)),       ACHAR(I=J, KIND=KIND(T%K1))) .AND. &
            (IACHAR(CC(J), KIND=KIND(T%K8)) .GE. IACHAR(CC(I), KIND=KIND(T%K8)))
    IF (.NOT. L) STOP 11
  END DO
  END DO



  END

