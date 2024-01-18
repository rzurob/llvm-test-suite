!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on KIND type parameter
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
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      INTERFACE FUNC
         INTEGER FUNCTION FUNC_BASE1(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(4,*)), INTENT(IN) :: Obj
         END FUNCTION

         INTEGER FUNCTION FUNC_BASE2(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(8,*)), INTENT(IN) :: Obj
         END FUNCTION
      END INTERFACE

      END MODULE Mod1
!*
      INTEGER FUNCTION FUNC_BASE1(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,*)), INTENT(IN) :: Obj

        SELECT TYPE (Obj)
          TYPE IS (NextGen(4,*,4,4))
             FUNC_BASE1 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,4,8))
             FUNC_BASE1 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,8,4))
             FUNC_BASE1 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(4,*,8,8))
             FUNC_BASE1 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (Child(4,*,4))
             FUNC_BASE1 = Obj%k1 + Obj%k2

          TYPE IS (Child(4,*,8))
             FUNC_BASE1 = Obj%k1 + Obj%k2

          TYPE IS (Base(4,*))
             FUNC_BASE1 = Obj%k1

          CLASS DEFAULT
           STOP 110
      END SELECT

      END FUNCTION

      INTEGER FUNCTION FUNC_BASE2(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(8,*)), INTENT(IN) :: Obj

        SELECT TYPE (Obj)
          TYPE IS (NextGen(8,*,4,4))
             FUNC_BASE2 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,4,8))
             FUNC_BASE2 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,8,4))
             FUNC_BASE2 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (NextGen(8,*,8,8))
             FUNC_BASE2 = Obj%k1 + Obj%k2 + Obj%k3

          TYPE IS (Child(8,*,4))
             FUNC_BASE2 = Obj%k1 + Obj%k2

          TYPE IS (Child(8,*,8))
             FUNC_BASE2 = Obj%k1 + Obj%k2

          TYPE IS (Base(8,*))
             FUNC_BASE2 = Obj%k1

          CLASS DEFAULT
           STOP 111
      END SELECT

      END FUNCTION

!*
      PROGRAM Generic_Interface01e
      USE MOD1
      IMPLICIT NONE

      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(Base(8,5))  :: base2

      CLASS(Base(4,:)), POINTER :: poly1
      CLASS(Child(8,:,4)), POINTER :: poly2

      IF( FUNC(base1) .NE. 4) STOP 08
      IF( FUNC(base2) .NE. 8) STOP 09

      ALLOCATE(Base(4,10):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) STOP 10

      IF( FUNC(poly1) .NE. 4) STOP 20

      poly1 => tgt1
      IF ( .NOT. ASSOCIATED(poly1)) STOP 12

      IF( FUNC(poly1) .NE. 8 ) STOP 22

      ALLOCATE(NextGen(4,10,8,8):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) STOP 13

      IF( FUNC(poly1) .NE. 20) STOP 23

      CALL SUB1(poly1)

      CALL SUB2(poly2)

      CONTAINS

      SUBROUTINE Sub1 (Arg)
      CLASS(Base(4,:)), POINTER ::  Arg

      ALLOCATE(Base(4,10):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) STOP 14

      IF( FUNC(Arg) .NE. 4) STOP 24

      Arg => tgt1
      IF ( .NOT. ASSOCIATED(Arg)) STOP 15

      IF( FUNC(Arg) .NE. 8) STOP 25

      ALLOCATE(NextGen(4,10,8,8):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) STOP 16

      IF( FUNC(Arg) .NE. 20) STOP 26

      END SUBROUTINE SUB1

      SUBROUTINE Sub2 (Arg)
      CLASS(Child(8,:,4)), POINTER :: Arg
      TYPE(Child(8,20,4)), TARGET :: tgt2

      Arg => tgt2
      IF ( .NOT. ASSOCIATED(Arg)) STOP 17

      IF( FUNC(Arg) .NE. 12) STOP 27

      END SUBROUTINE SUB2

      END PROGRAM Generic_Interface01e
