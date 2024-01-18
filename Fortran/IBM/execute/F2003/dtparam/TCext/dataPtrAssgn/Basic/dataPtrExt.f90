! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/dataPtrAssgn/Basic/dataPtrExt.f
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
!*  If no bounds-remapping-list is specified, the extent of a dimension of data-pointer-object
!*  is the extent of the corresponding dimension of data-target.
!*
!*  (323080)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K1,N1)    ! (4,20)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CLASS(DT0(K1,:)), POINTER :: Ptr(:,:)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT    ! (4,20)
  END TYPE

  TYPE (DT(4,20)), TARGET  :: T(1000,2)

  END MODULE

  PROGRAM dataPtrExt
  USE M
  IMPLICIT NONE


  CALL ModSub(T(1, 1))

  CONTAINS

  SUBROUTINE ModSub(Arg)
  TYPE(DT(4,*)), TARGET   :: Arg(1000,2)
  INTEGER            :: I

    DO I=1 ,1000
      Arg(I, 1)%Ptr(-999:, -1:) => Arg
      IF (ANY( LBOUND( Arg(I, 1)%Ptr) .NE. (/-999, -1/))) ERROR STOP 11
      IF (ANY( SHAPE(  Arg(I, 1)%Ptr) .NE. (/1000,  2/))) ERROR STOP 12
      Arg(I, 2)%Ptr(-499:500, -1:-1) => Arg(:, 2)
      IF (ANY( LBOUND( Arg(I, 2)%Ptr) .NE. (/-499, -1/))) ERROR STOP 21
      IF (ANY( SHAPE(  Arg(I, 2)%Ptr) .NE. (/1000,  1/))) ERROR STOP 22
    END DO

  END SUBROUTINE


  END


