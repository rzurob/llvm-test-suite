! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrPolyPtr.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 09, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Poly pointers
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrPolyPtr
  IMPLICIT NONE

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID
    CLASS(*), POINTER :: Ptr1(:, :)
    CLASS(*), POINTER :: Ptr2(:, :)
  END TYPE

  INTEGER  :: I, J
  TYPE(DT(4)), TARGET :: T(10,10)

  T%ID = RESHAPE((/(I, I=1,100)/), (/10,10/))

  DO J=1, 10
  DO I=1, 10
    T(I, J)%Ptr1(0:,0:) => T
    T(I, J)%Ptr2(0:9,1:1) => T(:, 10)
    !T(I, J)%Ptr1 => T
    !T(I, J)%Ptr2 => T(:, :)
  END DO
  END DO

  DO J=1, 10
  DO I=1, 10
    IF (.NOT. ASSOCIATED(T(I,J)%Ptr1, T))                    STOP 11
    IF (ANY( LBOUND(T(I,J)%Ptr1)         .NE. (/0, 0 /)))    STOP 12
    IF (ANY( UBOUND(T(I,J)%Ptr1)         .NE. (/9, 9 /)))    STOP 13
    SELECT TYPE(As => T(I,J)%Ptr1)
    TYPE IS(DT(4))
       IF (ANY( As%ID  .NE. T%ID))                           STOP 14
    CLASS DEFAULT
      STOP 15
    END SELECT

    IF (.NOT. ASSOCIATED(T(I,J)%Ptr2))                       STOP 21
    IF (ANY( LBOUND(T(I,J)%Ptr2)         .NE. (/0, 1 /)))    STOP 22
    IF (ANY( UBOUND(T(I,J)%Ptr2)         .NE. (/9, 1 /)))    STOP 23
    SELECT TYPE(As => T(I,J)%Ptr2)
    TYPE IS(DT(4))
       IF (ANY( As%ID  .NE. RESHAPE(T(:, 10)%ID, (/10,1/)))) STOP 24
    CLASS DEFAULT
      STOP 25
    END SELECT
  END DO
  END DO

  DO J=1, 10
  DO I=1, 10
    T(I, J)%Ptr1(0:,0:) => T(I, J)%Ptr2
    !T(I, J)%Ptr1 => T
  END DO
  END DO

  DO J=1, 1
  DO I=1, 9
    IF (.NOT. ASSOCIATED(T(I,J)%Ptr1, T(I,J)%Ptr2))          STOP 31
    IF (ANY( LBOUND(T(I,J)%Ptr1)         .NE. (/0, 0 /)))    STOP 32
    IF (ANY( UBOUND(T(I,J)%Ptr1)         .NE. (/9, 0 /)))    STOP 33
    SELECT TYPE(As => T(I,J)%Ptr1)
    TYPE IS(DT(4))
       IF (ANY( As%ID  .NE. RESHAPE(T(:, 10)%ID, (/10,1/)))) STOP 34
    CLASS DEFAULT
      STOP 35
    END SELECT
  END DO
  END DO


  END


