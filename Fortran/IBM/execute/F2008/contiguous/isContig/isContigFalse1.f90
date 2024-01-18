! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : -
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM isContigFalse1
      IMPLICIT NONE

      DOUBLE PRECISION, POINTER              :: ptr(:)
      DOUBLE PRECISION, ALLOCATABLE          :: all(:)

      DOUBLE PRECISION, POINTER              :: multi_ptr(:,:,:,:,:,:,:,:,:,:)
      DOUBLE PRECISION, ALLOCATABLE          :: multi_all(:,:,:,:,:,:,:,:,:,:)

      ALLOCATE( all(10) )
      all = -1.d0
      IF ( .NOT. IS_CONTIGUOUS(all) )  ERROR STOP 10

      ptr => foo(all(1:10:2))
      IF ( IS_CONTIGUOUS(ptr) )        ERROR STOP 11
      IF ( IS_CONTIGUOUS(foo(all(1:10:2))) ) ERROR STOP 12

!*************rank > 1

      ALLOCATE( multi_all(3,3,3,3,3,3,3,3,3,3) )
      multi_all = -10.d0
      IF ( .NOT. IS_CONTIGUOUS(multi_all) )  ERROR STOP 13

      multi_ptr => multi_foo(multi_all(:,:,:,:,:,:,:,:,:,1:3:2))
      IF ( IS_CONTIGUOUS(multi_ptr) )        ERROR STOP 14
      IF ( IS_CONTIGUOUS(multi_foo(multi_all(:,:,:,:,:,:,:,:,:,1:3:2))) )  ERROR STOP 15

      CONTAINS

      FUNCTION foo(Obj)
        DOUBLE PRECISION, TARGET  :: OBJ(:)
        DOUBLE PRECISION, POINTER :: foo(:)

            foo => Obj
      END FUNCTION foo

      FUNCTION multi_foo(Obj)
        DOUBLE PRECISION, TARGET  :: OBJ(:,:,:,:,:,:,:,:,:,:)
        DOUBLE PRECISION, POINTER :: multi_foo(:,:,:,:,:,:,:,:,:,:)

            multi_foo => Obj
      END FUNCTION multi_foo
END PROGRAM isContigFalse1
