!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
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
!* allocate-stmt is
!*   ALLOCATE ( [ type-spec :: ] allocation-list [, alloc-opt-list ] )
!*
!* C626 (R623) A type-param-value in a type-spec shall be an asterisk if and only if
!* each allocate-object is a dummy argument for which the corresponding type parameter is assumed.
!*
!* Defect 361730
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithTypeSpec12
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

      CLASS(Base(knd1,len1)), POINTER :: b1
      CLASS(Base(knd1,len2)), POINTER :: c2

      ALLOCATE(Base(knd1,len1) :: b1)
      IF ( b1%l1 .NE. len1) ERROR STOP 20

      b1%my_arr = (/(i, i = 1, len1)/)
      b1%my_type= 'Base'

      call allocate_auto(b1)

      ALLOCATE(Child(knd1,len2,knd1,len2) :: c2)

      SELECT TYPE ( c2 )
        CLASS IS (Child(knd1,*,knd1,*))
            IF ( c2%l1 .NE. len2) ERROR STOP 21
            IF ( c2%l2 .NE. len2) ERROR STOP 22
            c2%my_arr = (/(i, i = 1, len2)/)
            c2%my_type= 'Child'

            IF ( .NOT. ASSOCIATED(c2%c_cmp)) ALLOCATE(Base(knd1,c2%l2) :: c2%c_cmp)
            IF ( c2%c_cmp%l1 .NE. c2%l2) ERROR STOP 23
            IF ( c2%c_cmp%l1 .NE. len2) ERROR STOP 24
            c2%c_cmp%my_arr = (/(i, i = 1, len2)/)
            c2%c_cmp%my_type= 'Base'
            call allocate_auto(c2%c_cmp)

            IF ( .NOT. ASSOCIATED(c2%b_cmp)) ALLOCATE(Child(knd1,c2%l2,knd1,len1) :: c2%b_cmp)
            SELECT TYPE ( A => c2%b_cmp )
              CLASS IS (Child(knd1,*,knd1,*))
                 IF ( A%l1 .NE. c2%l2) ERROR STOP 25
                 IF ( A%l1 .NE. len2) ERROR STOP 26
                 IF ( A%l2 .NE. len1) ERROR STOP 27
                 A%my_arr = (/(i, i = 1, len2)/)
                 A%my_type= 'Child'
                 call allocate_auto(A)

              CLASS DEFAULT
                STOP 28
             END SELECT

        CLASS DEFAULT
           STOP 29
      END SELECT

      call allocate_auto(c2)

      DEALLOCATE(b1, c2)

      CONTAINS

      SUBROUTINE allocate_auto(Arg)
      CLASS(Base(4,*)) ::  Arg
      CLASS(Base(4,:)), POINTER ::  Obj

      SELECT TYPE ( Arg )
        CLASS IS (Base(knd1,*))
           ALLOCATE(Base(knd1,Arg%l1) :: Obj)
           IF ( .NOT. ASSOCIATED(Obj)) ERROR STOP 30
           IF ( SIZE(Obj%my_arr) .NE. Obj%l1) ERROR STOP 31
           IF ( LEN(Obj%my_type) .NE. 2*Obj%l1 ) ERROR STOP 32

        CLASS IS (Child(knd1,*,knd1,*))
           ALLOCATE(Child(knd1,Arg%l1,knd1,Arg%l2) :: Obj)
           IF ( .NOT. ASSOCIATED(Obj)) ERROR STOP 33
           IF ( SIZE(Obj%my_arr) .NE. Obj%l1) ERROR STOP 34
           IF ( LEN(Obj%my_type) .NE. 2*Obj%l1 ) ERROR STOP 35

        CLASS DEFAULT
           STOP 36
      END SELECT

      DEALLOCATE(Obj)

      END SUBROUTINE allocate_auto

END PROGRAM AllocateWithTypeSpec12
