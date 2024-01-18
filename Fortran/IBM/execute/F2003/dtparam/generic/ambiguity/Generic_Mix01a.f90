!*  ===================================================================
!*
!*                               DTP - Generic Type-Bound
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
!*  R448 type-bound-procedure-part is contains-stmt
!*                                     [ binding-private-stmt ]
!*                                     proc-binding-stmt
!*                                     [ proc-binding-stmt ] ...
!*
!*  R450 proc-binding-stmt is specific-binding
!*                         or generic-binding
!*                         or final-binding
!*
!*  R451 specific-binding is PROCEDURE [ (interface-name) ] &
!*                                    & [ [ , binding-attr -list ] :: ] &
!*                                    & binding-name [ => procedure-name ]
!*
!*  R452 generic-binding is GENERIC [, access-spec ] :: generic-spec => binding-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CONTAINS
         PROCEDURE, PASS :: FUNC_BASE1
         PROCEDURE, PASS :: FUNC_BASE2
         GENERIC :: FUNC =>  FUNC_BASE1, FUNC_BASE2
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2

      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3

      END TYPE NextGen

      CONTAINS
!*
      INTEGER FUNCTION FUNC_BASE1(Obj)
        INTEGER :: K, N
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
        INTEGER :: K, N
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

      END MODULE Mod1
!*
      PROGRAM Generic_Mix01a
      USE MOD1
      IMPLICIT NONE
      TYPE(Child(4,5,4)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(Base(8,5))  :: base2

      INTERFACE SUB
         SUBROUTINE Sub1(A)
           USE MOD1
           IMPLICIT NONE
           CLASS(Base(4,:)), POINTER ::  A
           TYPE(Child(4,5,4)), TARGET :: t
         END SUBROUTINE Sub1

         SUBROUTINE Sub2(A)
           USE MOD1
           IMPLICIT NONE
           CLASS(Child(8,:,4)), POINTER :: A
           TYPE(Child(8,20,4)), TARGET :: t
         END SUBROUTINE Sub2
      END INTERFACE

      CLASS(Base(4,:)), POINTER :: poly1
      CLASS(Child(8,:,4)), POINTER :: poly2

      IF( base1%FUNC () .NE. 4) STOP 08
      IF( base2%FUNC () .NE. 8) STOP 09

      ALLOCATE(Base(4,10):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) STOP 10

      IF(poly1%FUNC () .NE. 4) STOP 20

      poly1 => tgt1
      IF ( .NOT. ASSOCIATED(poly1)) STOP 12

      IF( poly1%FUNC () .NE. 8 ) STOP 22

      ALLOCATE(NextGen(4,10,8,8):: poly1)
      IF ( .NOT. ASSOCIATED(poly1)) STOP 13

      IF(poly1%FUNC () .NE. 20) STOP 23

      CALL SUB(poly1)

      CALL SUB(poly2)

      END PROGRAM Generic_Mix01a

      SUBROUTINE Sub1 (Arg)
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER ::  Arg
      TYPE(Child(4,5,4)), TARGET :: tgt1

      ALLOCATE(Base(4,10):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) STOP 14

      IF(Arg%FUNC () .NE. 4) STOP 24

      Arg => tgt1
      IF ( .NOT. ASSOCIATED(Arg)) STOP 15

      IF(Arg%FUNC () .NE. 8) STOP 25

      ALLOCATE(NextGen(4,10,8,8):: Arg)
      IF ( .NOT. ASSOCIATED(Arg)) STOP 16

      IF(Arg%FUNC () .NE. 20) STOP 26

      END SUBROUTINE SUB1

      SUBROUTINE Sub2 (Arg)
      USE MOD1
      IMPLICIT NONE

      CLASS(Child(8,:,4)), POINTER :: Arg
      TYPE(Child(8,20,4)), TARGET :: tgt2

      Arg => tgt2
      IF ( .NOT. ASSOCIATED(Arg)) STOP 17

      IF(Arg%FUNC () .NE. 12) STOP 27

      END SUBROUTINE SUB2
