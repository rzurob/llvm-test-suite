!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : November 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution for intrinsic function
!*                               based on type and kind
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

       INTEGER(k1), ALLOCATABLE :: base_arr(:)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2

        INTEGER(k2), ALLOCATABLE :: child_arr(:)
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3

        INTEGER(k3), ALLOCATABLE :: nextg_arr(:)
      END TYPE NextGen

      INTERFACE SIZE
        INTEGER FUNCTION SIZE_OBJ4(Obj)
          IMPORT Base, Child, NextGen
          CLASS(Base(4,*)) :: Obj
        END FUNCTION SIZE_OBJ4
      END INTERFACE

      END MODULE Mod1
!*
      MODULE Mod2
      USE MOD1
      IMPLICIT NONE

      INTERFACE SIZE
        INTEGER FUNCTION SIZE_OBJ8(Obj)
          IMPORT Base, Child, NextGen
          CLASS(Base(8,*)) :: Obj
        END FUNCTION SIZE_OBJ8
      END INTERFACE

      END MODULE Mod2
!*
      INTEGER FUNCTION SIZE_OBJ4(Obj)
        USE MOD1, Only: Base, Child, NextGen
        IMPLICIT NONE
        CLASS(Base(4,*)) :: Obj
        integer i

        SELECT TYPE ( Obj )
          TYPE IS (NextGen(4,*,4,4))
             Obj%base_arr  = (/ (5, I = 1, Obj%l1)/)
             Obj%child_arr = (/ (-55, I = 1, Obj%l1)/)
             Obj%nextg_arr =  (/ (55, I = 1, Obj%l1)/)

             size_obj4 = SIZE(Obj%base_arr) + SIZE(Obj%child_arr) + SIZE(Obj%nextg_arr)

          TYPE IS (Child(4,*,4))
             Obj%base_arr  = (/ (550, I = 1, Obj%l1)/)
             Obj%child_arr = (/ (-550, I = 1, Obj%l1)/)

             size_obj4 = SIZE(Obj%base_arr) + SIZE(Obj%child_arr)

          TYPE IS (Base(4,*))
             Obj%base_arr  = (/ (5000, I = 1, Obj%l1)/)

             size_obj4 = SIZE(Obj%base_arr)

          CLASS DEFAULT
           STOP 111
      END SELECT

      END FUNCTION SIZE_OBJ4

      INTEGER FUNCTION SIZE_OBJ8(Obj)
        USE MOD1, Only: Base, Child, NextGen
        IMPLICIT NONE
        CLASS(Base(8,*)) :: Obj
        integer i

        SELECT TYPE ( Obj )
          TYPE IS (NextGen(8,*,4,4))
             Obj%base_arr  = (/ (1, I = 1, Obj%k1)/)
             Obj%child_arr = (/ (-10, I = 1, Obj%k2)/)
             Obj%nextg_arr =  (/ (10, I = 1, Obj%k3)/)

             size_obj8 = SIZE(Obj%base_arr) + SIZE(Obj%child_arr) + SIZE(Obj%nextg_arr)

          TYPE IS (Child(8,*,4))
             Obj%base_arr  = (/ (100, I = 1, Obj%k1)/)
             Obj%child_arr = (/ (-100, I = 1, Obj%k2)/)

             size_obj8 = SIZE(Obj%base_arr) + SIZE(Obj%child_arr)

          TYPE IS (Base(8,*))
             Obj%base_arr  = (/ (1000, I = 1, Obj%k1)/)

             size_obj8 = SIZE(Obj%base_arr)

          CLASS DEFAULT
           STOP 112
      END SELECT

      END FUNCTION SIZE_OBJ8
!*
      PROGRAM Generic_Interface06
      USE MOD2
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: ptr1
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(NextGen(8,10,4,4)) :: dtv


      IF ( size(base1) .NE. size_obj4(base1) ) STOP 10
      IF ( size(base1) .NE. size(base1%base_arr) ) STOP 11

      ALLOCATE(Base(4,10):: ptr1)
      IF ( .NOT. ASSOCIATED(ptr1)) STOP 12

      SELECT TYPE (ptr1)
          TYPE IS (Base(4,*))
           IF (size(ptr1) .NE. size_obj4(ptr1)) STOP 13
           IF (size(ptr1) .NE. size(ptr1%base_arr)) STOP 14

          CLASS DEFAULT
           STOP 15
      END SELECT

      ptr1 => tgt1
      IF ( .NOT. ASSOCIATED(ptr1)) STOP 16

      SELECT TYPE (ptr1)
          TYPE IS (Child(4,*,4))
           IF (size(ptr1) .NE. size_obj4(ptr1)) STOP 17
           IF (size(ptr1) .NE. (size(ptr1%base_arr)+size(ptr1%child_arr)) )STOP 18

          CLASS DEFAULT
           STOP 19
      END SELECT

      IF ( size(dtv) .NE. size_obj8(dtv) ) STOP 20
      IF ( size(dtv) .NE. (size(dtv%base_arr)+size(dtv%child_arr)+size(dtv%nextg_arr)) ) STOP 21

      END PROGRAM Generic_Interface06
