! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : -
!*                               -
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
PROGRAM diagIsContig
      IMPLICIT NONE

      INTEGER                  :: val = 10
      INTEGER, PARAMETER       :: cte = KIND(0.0)
      DOUBLE PRECISION         :: var
      COMPLEX, TARGET          :: tgt
      CLASS(*), POINTER        :: ptr
      CLASS(*), ALLOCATABLE    :: all

      print *, IS_CONTIGUOUS(val) ! invalid
      print *, IS_CONTIGUOUS(cte) ! invalid
      print *, IS_CONTIGUOUS(var) ! invalid
      print *, IS_CONTIGUOUS(tgt) ! invalid

      ptr => tgt
      print *, IS_CONTIGUOUS(ptr) ! invalid

      ALLOCATE( REAL(8) :: all )
      print *, IS_CONTIGUOUS(all) ! invalid

      print *, IS_CONTIGUOUS(foo()) ! invalid

      CONTAINS

      INTEGER FUNCTION foo(Obj)
        INTEGER, OPTIONAL :: OBJ

            foo = -1
      END FUNCTION foo
END PROGRAM diagIsContig
