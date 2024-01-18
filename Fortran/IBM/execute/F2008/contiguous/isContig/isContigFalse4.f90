! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-03
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
PROGRAM isContigFalse4
      IMPLICIT NONE

      INTEGER, TARGET     :: tgt(10,10)
      INTEGER, POINTER    :: ptr(:,:) => NULL()
      INTEGER, POINTER, CONTIGUOUS :: ptrc(:,:) => NULL()

      IF ( .NOT. IS_CONTIGUOUS(tgt) )  ERROR STOP 10

      ptr => tgt

!Actual has TARGET attribute
      Call Sub_assumed_shape(tgt(:,1:10:2))
      Call Sub_assumed_shape(tgt(1:2,1:10:1))

      Call Sub_assumed_size(tgt(:,1:10:2))
      Call Sub_assumed_size(tgt(1,1:10:2))
      Call Sub_assumed_size(tgt(1:2,1:10:2))

!Actual has POINTER attribute
      Call Sub_assumed_shape(ptr(:,1:10:2))
      Call Sub_assumed_shape(ptr(1:2,1:10:1))

      Call Sub_assumed_size(ptr(:,1:10:2))
      Call Sub_assumed_size(ptr(1,1:10:2))
      Call Sub_assumed_size(ptr(1:2,1:10:2))

      ptrc => ptr
!Actual has POINTER and CONTIGUOUS attributes
      Call Sub_assumed_shape(ptrc(:,1:10:2))
      Call Sub_assumed_shape(ptrc(1:2,1:10:1))

      Call Sub_assumed_size(ptrc(:,1:10:2))
      Call Sub_assumed_size(ptrc(1,1:10:2))
      Call Sub_assumed_size(ptrc(1:2,1:10:2))

      CONTAINS

      SUBROUTINE Sub_assumed_shape(Arg)
        INTEGER :: I, Arg(:,:)                       ! Assumed shape array : the memory layout depends on the actual argument
                                                     ! If actual argument is contiguous => dummy is contiguous
                                                     ! If actual argument is not contiguous => dummy is not contiguous
        IF ( IS_CONTIGUOUS(Arg) )  ERROR STOP 20

        DO I = 1, 10
          IF ( .NOT. IS_CONTIGUOUS(tgt(:,I)) )  ERROR STOP 21
          Call Sub_contig(tgt(:,I))                  ! contiguous
        ENDDO

        DO I = 1, 10
          IF ( IS_CONTIGUOUS(tgt(I,:)) )        ERROR STOP 22
          Call Sub_not_contig(tgt(I,:))              ! not contiguous
        ENDDO
      END SUBROUTINE Sub_assumed_shape

      SUBROUTINE Sub_contig(Arg)
        INTEGER, CONTIGUOUS :: Arg(:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) )         ERROR STOP 23
      END SUBROUTINE Sub_contig

      SUBROUTINE Sub_not_contig(Arg)
        INTEGER :: Arg(:)

        IF ( IS_CONTIGUOUS(Arg) )               ERROR STOP 24
      END SUBROUTINE Sub_not_contig

      SUBROUTINE Sub_assumed_size(Arg)
        INTEGER :: Arg(*)                            ! Assumed size arrays are always stored in contiguous memory
                                                     ! the compiler makes a copy of the actual argument if it is not contiguous
        IF ( .NOT. IS_CONTIGUOUS(Arg) )         ERROR STOP 25
      END SUBROUTINE Sub_assumed_size
END PROGRAM isContigFalse4
