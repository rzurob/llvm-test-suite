! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
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
PROGRAM isContigTrue6
      IMPLICIT NONE

      REAL(16), POINTER              :: ptr(:)
      REAL(16), ALLOCATABLE          :: all(:)

      REAL(16), POINTER              :: Mptr(:,:,:,:,:,:,:,:,:,:)
      REAL(16), ALLOCATABLE          :: Mall(:,:,:,:,:,:,:,:,:,:)

      ALLOCATE( all(10) )
      all = -1.
      IF ( .NOT. IS_CONTIGUOUS(all) )  ERROR STOP 10

      ptr => foo(all(3:9:1))
      IF ( .NOT. IS_CONTIGUOUS(ptr) )             ERROR STOP 11
      IF ( .NOT. IS_CONTIGUOUS(foo(all(3:9:1))) ) ERROR STOP 12

      ptr => foo(all(1:0))
      IF ( .NOT. IS_CONTIGUOUS(ptr) )           ERROR STOP 13
      IF ( .NOT. IS_CONTIGUOUS(foo(all(1:0))) ) ERROR STOP 14

      ptr => foo(all(1:2:2))
      IF ( .NOT. IS_CONTIGUOUS(ptr) )             ERROR STOP 15
      IF ( .NOT. IS_CONTIGUOUS(foo(all(1:2:2))) ) ERROR STOP 16

!*************rank > 1

      ALLOCATE( Mall(3,3,3,3,3,3,3,3,3,3) )
      Mall = -10.d0
      IF ( .NOT. IS_CONTIGUOUS(Mall) )  ERROR STOP 20

      Mptr => Mfoo(Mall(:,:,:,:,:,:,:,:,:,1:3:1))
      IF ( .NOT. IS_CONTIGUOUS(Mptr) )                                 ERROR STOP 21
      IF ( .NOT. IS_CONTIGUOUS(Mfoo(Mall(:,:,:,:,:,:,:,:,:,1:3:1))) )  ERROR STOP 22

      IF ( .NOT. IS_CONTIGUOUS(Mptr) )                                   ERROR STOP 23
      IF ( .NOT. IS_CONTIGUOUS(Mfoo(Mall(:,:,:,:,1:3,:,:,1:3:1,:,:))) )  ERROR STOP 24

      CONTAINS

      FUNCTION foo(Obj)
        REAL(16), TARGET, CONTIGUOUS  :: OBJ(:)
        REAL(16), POINTER, CONTIGUOUS :: foo(:)

            foo => Obj
      END FUNCTION foo

      FUNCTION Mfoo(Obj)
        REAL(16), TARGET, CONTIGUOUS  :: OBJ(:,:,:,:,:,:,:,:,:,:)
        REAL(16), POINTER, CONTIGUOUS :: Mfoo(:,:,:,:,:,:,:,:,:,:)

            Mfoo => Obj
      END FUNCTION Mfoo
END PROGRAM isContigTrue6
