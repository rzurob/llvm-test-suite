! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Bound remapping
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : -
!*
!*    LHS is array pointer with CONTIGUOUS attribute (rank =3)
!*    RHS is allocatable allocated array (rank =1)
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
PROGRAM bndMapping1
      IMPLICIT NONE

      INTEGER :: I, u1, u2, u3
      INTEGER, POINTER, CONTIGUOUS  :: ptrc(:,:,:)
      INTEGER, ALLOCATABLE, TARGET  :: Iarr(:)

      ALLOCATE( Iarr(1000), SOURCE =[(I, I=1,1000)] )

      ptrc(1:10,1:10,1:10) => Iarr
      IF ( ANY(ptrc .NE. RESHAPE(SOURCE=[(I, I=1,1000)], SHAPE=[10,10,10])) ) ERROR STOP 31

      u1 = 10
      u2 = 10
      u3 = 10
      IF ( (u1*u2*u3) .GT. size(Iarr) ) ERROR STOP 10
      ptrc(1:u1,1:u2,1:u3) => Iarr
      IF ( ANY(ptrc .NE. RESHAPE(SOURCE=[(I, I=1,1000)], SHAPE=[10,10,10])) ) ERROR STOP 31

      ptrc(1:10,1:1,1:1) => Iarr
      IF ( ANY(ptrc .NE. RESHAPE(SOURCE=[(I, I=1,10)], SHAPE=[10,1,1])) ) ERROR STOP 31

      ptrc(1:5,1:5,1:5) => Iarr
      IF ( ANY(ptrc .NE. RESHAPE(SOURCE=[(I, I=1,125)], SHAPE=[5,5,5])) ) ERROR STOP 31

END PROGRAM bndMapping1
