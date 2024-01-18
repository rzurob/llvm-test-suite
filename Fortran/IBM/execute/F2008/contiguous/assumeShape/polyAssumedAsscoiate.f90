! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic
!*  SECONDARY FUNCTIONS TESTED : - ASSOCIATE construct
!*                               - polymorphic contiguous assumed
!*                                 shape array dummy argument
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
MODULE Mod
      IMPLICIT NONE

      TYPE :: Base(K0, L0)
        INTEGER, KIND :: K0 = 4
        INTEGER, LEN  :: L0 = 10

        INTEGER(K0) :: I0(K0) = -1
      END TYPE

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER :: I
        CLASS(Base(4,*)), CONTIGUOUS :: Arg(:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 10

        DO I = 1, SIZE(Arg)
           ASSOCIATE( a => Arg(I) )
             IF ( .NOT.  IS_CONTIGUOUS(a%I0) )  ERROR STOP 11
             IF ( a%K0       .NE.   4 ) ERROR STOP 12
             IF ( a%L0       .NE.  10 ) ERROR STOP 13
             IF ( SIZE(a%I0) .NE.   4 ) ERROR STOP 14
             IF ( ANY(a%I0   .NE. -1) ) ERROR STOP 15
           END ASSOCIATE
        END DO
      END SUBROUTINE Sub

END MODULE
PROGRAM polyAssumedAsscoiate
      USE Mod
      IMPLICIT NONE

      TYPE(Base), TARGET             :: Iarr(10)
      CLASS(Base(4,:)), POINTER      :: ptr(:)
      CLASS(Base), ALLOCATABLE       :: upoly(:)

      CALL Sub(Iarr)
      CALL Sub(Iarr(:5))
      CALL Sub(Iarr(:5:1))

      ptr => Iarr
      CALL Sub(ptr)
      CALL Sub(ptr(:5))
      CALL Sub(ptr(:5:1))

      ALLOCATE( upoly(10), SOURCE = Iarr )
      CALL Sub(upoly)
      CALL Sub(upoly(:5))
      CALL Sub(upoly(:5:1))
END PROGRAM polyAssumedAsscoiate
