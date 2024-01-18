!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!* Defect : 359547
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

       TYPE Base (k1,l1)
         INTEGER, KIND :: k1
         INTEGER, LEN  :: l1

         INTEGER(k1) :: A1(l1)
       END TYPE

       TYPE :: Container (k2,l2)
         INTEGER, KIND :: k2
         INTEGER, LEN  :: l2

         TYPE(Base(k2,l2)), ALLOCATABLE :: poly(:)
         CONTAINS

         procedure :: assgnBase
         generic :: assignment(=) => assgnBase
      END TYPE

       CONTAINS

      RECURSIVE SUBROUTINE AssgnBase (this, arg)
        CLASS(Container(4,*)), INTENT(inout) :: this
        TYPE(Base(4,*)),  INTENT(in)  :: arg(:)
        TYPE(Base(4,:)), ALLOCATABLE, SAVE :: temp(:)

        IF (SIZE(arg) .GE. 1) then
            IF (.NOT. ALLOCATED(temp)) then
                temp = arg
            ELSE
                temp = [temp, arg]
            END IF

            this%poly = temp
            this = arg(:size(arg) -1)
        ELSE
            call MOVE_ALLOC(temp, this%poly)
        END IF

    END SUBROUTINE
END MODULE
PROGRAM DTP_ACE_11
      USE Mod
      IMPLICIT NONE
      INTEGER :: I
      TYPE(Container(4,:)), ALLOCATABLE :: c1
      TYPE(Base(4,:)), ALLOCATABLE :: b1(:)

      ALLOCATE (Container(4,10) :: c1)
      IF ( .NOT. ALLOCATED(c1) ) STOP 10

      b1 = [Base(4,10) :: Base(4,10)([(I, I=1, 10)]), Base(4,10)([(I+10, I=1, 10)]), Base(4,10)([(I+20, I=1, 10)])]
      IF ( .NOT. ALLOCATED(b1) ) STOP 11
      IF ( SIZE(b1)    .NE.  3 ) STOP 12
      DO I = 1, SIZE(b1)
         IF ( SIZE(b1(I)%A1) .NE. 10 ) STOP 13
      END DO

      c1 = b1
      IF ( .NOT. ALLOCATED(c1%poly) ) STOP 14
      IF ( SIZE(c1%poly)    .NE.  6 ) STOP 15
      DO I = 1, SIZE(c1%poly)
         IF ( SIZE(c1%poly(I)%A1) .NE. 10 ) STOP 16
      END DO
      print*, c1%poly

      c1 = b1(3:1:-2)
      IF ( .NOT. ALLOCATED(c1%poly) ) STOP 17
      IF ( SIZE(c1%poly)    .NE.  3 ) STOP 18
      DO I = 1, SIZE(c1%poly)
         IF ( SIZE(c1%poly(I)%A1) .NE. 10 ) STOP 19
      END DO
      print*, c1%poly

END PROGRAM DTP_ACE_11
