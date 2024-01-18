!* ===================================================================
!*
!* DATE                       : June 2, 2015
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
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
!* Defect 361745 and 361730
!*
!* TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/SourceExp/AllocateWithSourceExp04.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp04!-08
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 1

        CHARACTER(2*l1)  :: my_type
        INTEGER(KIND=k1) :: my_arr(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 2

        CLASS(Base(k2,l2)), POINTER :: b_cmp
        CLASS(Base(k2,l2)), POINTER :: c_cmp
      END TYPE Child

      INTEGER, PARAMETER :: knd1 = KIND(0), len1 = 10, len2 = 5
      INTEGER :: I, J

      CLASS(Base(knd1, len1)), POINTER :: b1, b2
      CLASS(Base(knd1, len2)), POINTER :: c2

      ALLOCATE(Base(knd1, len1) :: b1, b2)
      IF ( b1%l1 .NE. len1) ERROR STOP 20

      b1%my_arr = (/(2, i = 1, len1)/)
      b1%my_type= 'Base'

      call allocate_auto(b1)

      ALLOCATE(Child(knd1,len2,knd1,len2) :: c2)

      SELECT TYPE ( c2 )
        CLASS IS (Child(knd1,*,knd1,*))
            IF ( c2%l1 .NE. len2) ERROR STOP 21
            IF ( c2%l2 .NE. len2) ERROR STOP 22
            c2%my_arr = (/(5, i = 1, len2)/)
            c2%my_type= 'Child'

            IF ( .NOT. ASSOCIATED(c2%c_cmp)) ALLOCATE(Base(knd1,c2%l2) :: c2%c_cmp)
            IF ( c2%c_cmp%l1 .NE. c2%l2) ERROR STOP 23
            IF ( c2%c_cmp%l1 .NE. len2) ERROR STOP 24
            c2%c_cmp%my_arr = (/(2, i = 1, len2)/)
            c2%c_cmp%my_type= 'Base'
            call allocate_auto(c2%c_cmp)

            IF ( .NOT. ASSOCIATED(c2%b_cmp)) ALLOCATE(Child(knd1,c2%l2,knd1,len1) :: c2%b_cmp)
            SELECT TYPE ( A => c2%b_cmp )
              CLASS IS (Child(knd1,*,knd1,*))
                 IF ( A%l1 .NE. c2%l2) ERROR STOP 25
                 IF ( A%l1 .NE. len2) ERROR STOP 26
                 IF ( A%l2 .NE. len1) ERROR STOP 27
                 A%my_arr = (/(5, i = 1, len2)/)
                 A%my_type= 'Child'
                 call allocate_auto(A)

              CLASS DEFAULT
                ERROR STOP 28
             END SELECT

        CLASS DEFAULT
           ERROR STOP 29
      END SELECT

      call allocate_auto(c2)

      DEALLOCATE(b1, c2)

      CONTAINS
      SUBROUTINE allocate_auto(Arg)
      CLASS(Base(knd1,*)) ::  Arg
      CLASS(Base(knd1,:)), POINTER ::  Obj1, Obj2

      ALLOCATE(Obj1, Obj2, SOURCE = Arg)
      IF ( .NOT. ASSOCIATED(Obj1)) ERROR STOP 30
      IF ( .NOT. ASSOCIATED(Obj2)) ERROR STOP 300

      SELECT TYPE ( Obj1 )
        CLASS IS (Base(knd1,*))

           IF ( SIZE(Obj1%my_arr) .NE. Obj1%l1) ERROR STOP 31
           IF ( ANY(Obj1%my_arr .NE. 2) ) ERROR STOP 32
           IF ( Obj1%my_type .NE. 'Base' ) ERROR STOP 33

        CLASS IS (Child(knd1,*,knd1,*))
           IF ( SIZE(Obj1%my_arr) .NE. Obj1%l1) ERROR STOP 34
           IF ( ANY(Obj1%my_arr .NE. 5) ) ERROR STOP 35
           IF ( Obj1%my_type .NE. 'Child' ) ERROR STOP 36

        CLASS DEFAULT
           ERROR STOP 37
      END SELECT

      DEALLOCATE(Obj1)
      END SUBROUTINE allocate_auto
END PROGRAM AllocateWithSourceExp04!-08
