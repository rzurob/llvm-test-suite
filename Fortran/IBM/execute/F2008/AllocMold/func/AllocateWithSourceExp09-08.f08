!* ===================================================================
!*
!* DATE                       : June 2, 2015
!*
!* PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with Source Expression
!* SECONDARY FUNCTIONS TESTED :
!*
!* REQUIRED COMPILER OPTIONS  :
!*
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* NUMBER OF TESTS CONDITIONS :
!*
!* DESCRIPTION                :
!*
!* Defect 359514
!*
!* TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/SourceExp/AllocateWithSourceExp09.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp09
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10

        CHARACTER(l1)  :: Carr(l1) = 'ABCDEFGHIJ'
        INTEGER(k1) :: Iarr(l1) = 1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10

        CLASS(Base(k2,:)), POINTER :: b_cmp
      END TYPE Child

      CLASS(*), POINTER :: b1, c1, d1

      ALLOCATE(Base(4,9) :: b1)

      SELECT TYPE(b1)
        TYPE IS(Base(4,*))
           IF(b1%k1 .NE. 4) ERROR STOP 10
           IF(b1%l1 .NE. 9) ERROR STOP 11
           IF(ANY(b1%Carr .NE. 'ABCDEFGHI')) ERROR STOP 12
           IF(ANY(b1%Iarr .NE. 1)) ERROR STOP 13
           ALLOCATE(c1, d1, SOURCE = Child(4,7,4,9)(b_cmp = b1) )

        CLASS DEFAULT
            ERROR STOP 101
      END SELECT

      SELECT TYPE(c1)
        TYPE IS(Child(4,*,4,*))
           IF(c1%k1 .NE. 4) ERROR STOP 14
           IF(c1%l1 .NE. 7) ERROR STOP 15
           IF(c1%k2 .NE. 4) ERROR STOP 16
           IF(c1%l2 .NE. 9) ERROR STOP 17
           IF(ANY(c1%Carr .NE. 'ABCDEFG')) ERROR STOP 18
           IF(ANY(c1%Iarr .NE. 1)) ERROR STOP 19
           IF(c1%b_cmp%k1 .NE. 4) ERROR STOP 20
           IF(c1%b_cmp%l1 .NE. 9) ERROR STOP 21
           IF(ANY(c1%b_cmp%Carr .NE. 'ABCDEFGHI')) ERROR STOP 22
           IF(ANY(c1%b_cmp%Iarr .NE. 1)) ERROR STOP 23

        CLASS DEFAULT
            ERROR STOP 102
      END SELECT

      SELECT TYPE(d1)
        TYPE IS(Child(4,*,4,*))
          IF(d1%k1 .NE. 4) ERROR STOP 44
          IF(d1%l1 .NE. 7) ERROR STOP 45
          IF(d1%k2 .NE. 4) ERROR STOP 46
          IF(d1%l2 .NE. 9) ERROR STOP 47
          IF(ANY(d1%Carr .NE. 'ABCDEFG')) ERROR STOP 48
          IF(ANY(d1%Iarr .NE. 1)) ERROR STOP 49
          IF(d1%b_cmp%k1 .NE. 4) ERROR STOP 50
          IF(d1%b_cmp%l1 .NE. 9) ERROR STOP 51
          IF(ANY(d1%b_cmp%Carr .NE. 'ABCDEFGHI')) ERROR STOP 52
          IF(ANY(d1%b_cmp%Iarr .NE. 1)) ERROR STOP 53

        CLASS DEFAULT
            ERROR STOP 103
      END SELECT
!     DEALLOCATE(d1)

      ALLOCATE(b1, d1, SOURCE = c1)
      SELECT TYPE(b1)
         TYPE IS(Child(4,*,4,*))
           IF(b1%k1 .NE. 4) ERROR STOP 24
           IF(b1%l1 .NE. 7) ERROR STOP 25
           IF(b1%k2 .NE. 4) ERROR STOP 26
           IF(b1%l2 .NE. 9) ERROR STOP 27
           IF(ANY(b1%Carr .NE. 'ABCDEFG')) ERROR STOP 28
           IF(ANY(b1%Iarr .NE. 1)) ERROR STOP 29
           IF(b1%b_cmp%k1 .NE. 4) ERROR STOP 30
           IF(b1%b_cmp%l1 .NE. 9) ERROR STOP 31
           IF(ANY(b1%b_cmp%Carr .NE. 'ABCDEFGHI')) ERROR STOP 32
           IF(ANY(b1%b_cmp%Iarr .NE. 1)) ERROR STOP 33

        CLASS DEFAULT
            ERROR STOP 104
      END SELECT

      SELECT TYPE(d1)
        TYPE IS(Child(4,*,4,*))
          IF(d1%k1 .NE. 4) ERROR STOP 54
          IF(d1%l1 .NE. 7) ERROR STOP 55
          IF(d1%k2 .NE. 4) ERROR STOP 56
          IF(d1%l2 .NE. 9) ERROR STOP 57
          IF(ANY(d1%Carr .NE. 'ABCDEFG')) ERROR STOP 58
          IF(ANY(d1%Iarr .NE. 1)) ERROR STOP 59
          IF(d1%b_cmp%k1 .NE. 4) ERROR STOP 60
          IF(d1%b_cmp%l1 .NE. 9) ERROR STOP 61
          IF(ANY(d1%b_cmp%Carr .NE. 'ABCDEFGHI')) ERROR STOP 62
          IF(ANY(d1%b_cmp%Iarr .NE. 1)) ERROR STOP 63

        CLASS DEFAULT
           ERROR STOP 105
      END SELECT

      DEALLOCATE(b1, c1, d1)

END PROGRAM AllocateWithSourceExp09
