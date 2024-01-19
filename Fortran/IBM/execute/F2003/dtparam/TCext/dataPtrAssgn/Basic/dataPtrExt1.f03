! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/Basic/dataPtrExt1.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 07, 2006
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
!*  If bounds-spec-list appears, it specifies the lower bounds; otherwise, the lower bound
!*  of each dimension is the result of the intrinsic function LBOUND (13.7.60)
!*  applied to the corresponding dimension of data-target. The upper bound of each dimension
!*  is one less than the sum of the lower bound and the extent.
!*
!*  - assumed-size array
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K1,N1)    ! (4,20)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CLASS(DT0(K1,:)), POINTER :: Ptr(:,:)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(K2,N2)    ! (4,20,4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
  END TYPE

  TYPE (DT(4,20,4,20)), TARGET  :: T(1000,2)
  TYPE (DT(4,:,4,:)), POINTER  :: Ptr(:, :)

  END MODULE

  PROGRAM dataPtrExt1
  USE M
  IMPLICIT NONE


  CALL IntSub(T)

  Ptr(0:, 0:) => T
  Ptr(1:, 1:) => Ptr

  IF (ANY( LBOUND( Ptr) .NE. (/1,   1/))) ERROR STOP 31
  IF (ANY( UBOUND( Ptr) .NE. (/1000,2/))) ERROR STOP 32

  CONTAINS

  SUBROUTINE IntSub(Arg)
  TYPE(DT(4,*,4,*)), TARGET   :: Arg(1000,2:*)
  INTEGER            :: I

    DO I=1 ,1000
      Arg(I, 2)%Ptr(-999:, -1:) => Arg(::2,:3)
      IF (ANY( LBOUND( Arg(I, 2)%Ptr) .NE. (/-999, -1/))) ERROR STOP 11
      IF (ANY( SHAPE(  Arg(I, 2)%Ptr) .NE. (/ 500,  2/))) ERROR STOP 12
      Arg(I, 3)%Ptr(-499:500, -1:-1) => Arg(:, 2)
      IF (ANY( LBOUND( Arg(I, 3)%Ptr) .NE. (/-499, -1/))) ERROR STOP 21
      IF (ANY( SHAPE(  Arg(I, 3)%Ptr) .NE. (/1000,  1/))) ERROR STOP 22
    END DO

  END SUBROUTINE


  END


