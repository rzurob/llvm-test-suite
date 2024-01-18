! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/kindArg/kindArg/kindArgIchar8.f
! opt variations: -qnol -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIchar8
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 12, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR 
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
!*  For any characters C and D capable of  representation in the processor, 
!*  C <= D is true if and only if ICHAR (C) <= ICHAR (D) is true
!*  and C == D is true if and only if ICHAR (C) == ICHAR (D) is true 
!*    
!*  (324718) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIchar8
  IMPLICIT NONE

  INTEGER(1) :: I1, II1(128), K1
  INTEGER(2) :: I2, II2(128), K2
  INTEGER(4) :: I4, II4(128), K4
  INTEGER(8) :: I8, II8(128), K8

  INTEGER   :: I, J
  LOGICAL   :: L
  CHARACTER(LEN=1), POINTER :: CC(:)
  
  TYPE :: DT(N1,D1)    ! (20,1)
    INTEGER, KIND :: D1
    INTEGER, LEN  :: N1
    INTEGER(D1)   :: K1=0
    INTEGER(D1)   :: K2=0
    INTEGER(D1)   :: K4=0
    INTEGER(D1)   :: K8=0
  END TYPE

  TYPE (DT(20,1)), PARAMETER :: T(128)=DT(20,1)(1,2,4,8)

  ALLOCATE(CC(0:127), SOURCE=(/(CHAR(I), I=0,127)/))

  DO I = 0, 127 
  DO J = I, 127 
    L = CC(I) .LE. CC(J) .AND. &
            (ICHAR(CC(I), KIND=KIND(T%K1)) .LE. ICHAR(CC(J), KIND=KIND(T%K1)))
    IF (.NOT. L) STOP 11
  END DO
  END DO

  DO I = 0, 127 
  DO J = I, 127 
    L = CC(I) .LE. CC(J) .AND. &
            (ICHAR(CC(I), KIND=KIND(T%K2)) .LE. ICHAR(CC(J), KIND=KIND(T%K2)))
    IF (.NOT. L) STOP 12
  END DO
  END DO

  DO I = 0, 127 
  DO J = I, 127 
    L = CC(I) .LE. CC(J) .AND. &
            (ICHAR(CC(I), KIND=KIND(T%K4)) .LE. ICHAR(CC(J), KIND=KIND(T%K4)))
    IF (.NOT. L) STOP 13
  END DO
  END DO

  DO I = 0, 127 
  DO J = I, 127 
    L = CC(I) .LE. CC(J) .AND. &
            (ICHAR(CC(I), KIND=KIND(T%K8)) .LE. ICHAR(CC(J), KIND=KIND(T%K8)))
    IF (.NOT. L) STOP 14
  END DO
  END DO

  DO I = 0, 127 
    L = CC(I) .EQ. CC(I) .AND. &
        (ICHAR(CC(I), KIND=T(2)%K1) .EQ. ICHAR(CC(I), KIND=T(1)%K1))
    IF (.NOT. L) STOP 15
  END DO

  DO I = 0, 127 
    L = CC(I) .EQ. CC(I) .AND. &
        (ICHAR(CC(I), KIND=T(6)%K2) .EQ. ICHAR(CC(I), KIND=T(3)%K2))
    IF (.NOT. L) STOP 16 
  END DO

  DO I = 0, 127 
    L = CC(I) .EQ. CC(I) .AND. &
        (ICHAR(CC(I), KIND=T(10)%K4) .EQ. ICHAR(CC(I), KIND=T(5)%K4))
    IF (.NOT. L) STOP 17 
  END DO

  DO I = 0, 127 
    L = CC(I) .EQ. CC(I) .AND. &
        (ICHAR(CC(I), KIND=T(12)%K8) .EQ. ICHAR(CC(I), KIND=T(7)%K8))
    IF (.NOT. L) STOP 18 
  END DO



  END

