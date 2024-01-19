!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBTIONS TESTED : Resolution based on KIND type parameter
!*                               polymorphic objects
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
        INTEGER, LEN :: l1

        INTEGER(k1) :: sum
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      INTERFACE SUB
        SUBROUTINE SUB_BASE1(Obj)
          import Base, Child, NextGen
          CLASS(Base(4,*)), INTENT(INOUT) :: Obj
        END SUBROUTINE

        SUBROUTINE SUB_BASE2(Obj)
          import Base, Child, NextGen
          CLASS(Base(8,*)), INTENT(INOUT) :: Obj
        END SUBROUTINE
      END INTERFACE

      END MODULE Mod1
!*
      SUBROUTINE SUB_BASE1(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,*)), INTENT(INOUT) :: Obj

        SELECT TYPE (Obj)
          TYPE IS (NextGen(4,*,4,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,4,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,8,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,8,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (Child(4,*,4))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Child(4,*,8))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Base(4,*))
             Obj%sum = Obj%k1

          CLASS DEFAULT
           STOP 110
      END SELECT

      END SUBROUTINE

      SUBROUTINE SUB_BASE2(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(8,*)), INTENT(INOUT) :: Obj

        SELECT TYPE (Obj)
          TYPE IS (NextGen(8,*,4,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,4,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,8,4))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,8,8))
             Obj%sum = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (Child(8,*,4))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Child(8,*,8))
             Obj%sum = Obj%k1 + Obj%k2

          TYPE IS (Base(8,*))
             Obj%sum = Obj%k1

          CLASS DEFAULT
           STOP 111
      END SELECT

      END SUBROUTINE
!*
      PROGRAM Generic_Interface02b
      USE MOD1
      IMPLICIT NONE

      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(Base(8,5))  :: base2

      CLASS(Base(4,:)), POINTER :: poly1
      CLASS(Child(8,:,4)), POINTER :: poly2

      CALL SUB(base1)
      IF( base1%sum .NE. 4) ERROR STOP 10

      CALL SUB(base2)
      IF( base2%sum .NE. 8) ERROR STOP 11

      ALLOCATE(Base(4,10):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) ERROR STOP 12

      CALL SUB(poly1)
      IF( poly1%sum .NE. 4) ERROR STOP 13

      poly1 => tgt1
      IF ( .NOT. ASSOCIATED(poly1)) ERROR STOP 14

      CALL SUB(poly1)
      IF( poly1%sum .NE. 8) ERROR STOP 15

      ALLOCATE(NextGen(4,10,8,8):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) ERROR STOP 16

      CALL SUB(poly1)
      IF( poly1%sum .NE. 20) ERROR STOP 17

      CALL SUB1(poly1)

      CALL SUB2(poly2)

      CONTAINS

      SUBROUTINE Sub1 (Arg)
      CLASS(Base(4,:)), POINTER ::  Arg

      ALLOCATE(Base(4,10):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 20

      CALL SUB(Arg)
      IF( Arg%sum .NE. 4) ERROR STOP 21

      Arg => tgt1
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 22

      CALL SUB(Arg)
      IF( Arg%sum .NE. 8) ERROR STOP 23

      ALLOCATE(NextGen(4,10,8,8):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 24

      CALL SUB(Arg)
      IF( Arg%sum .NE. 20) ERROR STOP 25

      END SUBROUTINE SUB1

      SUBROUTINE Sub2 (Arg)
      CLASS(Child(8,:,4)), POINTER :: Arg
      TYPE(Child(8,20,4)), TARGET :: tgt2

      Arg => tgt2
      IF ( .NOT. ASSOCIATED(Arg)) ERROR STOP 17

      CALL SUB(Arg)
      IF( Arg%sum .NE. 12) ERROR STOP 18

      END SUBROUTINE SUB2

      END PROGRAM Generic_Interface02b
