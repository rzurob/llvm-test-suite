!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution based on rank
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

      INTERFACE FUNC
        CLASS(Base(4,:)) FUNCTION foo1(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(4,:)), POINTER :: Obj     ! rank 0
           POINTER :: foo1
         END FUNCTION foo1

         CLASS(Base(4,:)) FUNCTION foo2(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(4,:)), POINTER :: Obj(:,:) ! rank 2
           POINTER :: foo2
         END FUNCTION foo2
      END INTERFACE

      END MODULE Mod1
!*
      CLASS(Base(4,:)) FUNCTION foo1(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,:)), POINTER :: Obj
        POINTER :: foo1

        foo1 => Obj
        IF ( .NOT. ASSOCIATED(foo1)) ERROR STOP 100

        SELECT TYPE ( foo1 )
          TYPE IS (NextGen(4,*,4,4))
             foo1%base_arr  = (/ (1, I = 1, foo1%l1)/)
             foo1%child_arr = (/ (-10, I = 1, foo1%l1)/)
             foo1%nextg_arr =  (/ (10, I = 1, foo1%l1)/)

          TYPE IS (Child(4,*,4))
             foo1%base_arr  = (/ (100, I = 1, foo1%l1)/)
             foo1%child_arr = (/ (-100, I = 1, foo1%l1)/)

          TYPE IS (Base(4,*))
             foo1%base_arr  = (/ (1000, I = 1, foo1%l1)/)

          CLASS DEFAULT
           STOP 110
      END SELECT

      END FUNCTION foo1

      CLASS(Base(4,:)) FUNCTION foo2(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,:)), POINTER :: Obj(:,:)
        POINTER :: foo2

        foo2 => Obj(1,1)
        IF ( .NOT. ASSOCIATED(foo2)) ERROR STOP 101

        SELECT TYPE ( foo2 )
          TYPE IS (NextGen(4,*,4,4))
             foo2%base_arr  = (/ (5, I = 1, foo2%l1)/)
             foo2%child_arr = (/ (-55, I = 1, foo2%l1)/)
             foo2%nextg_arr =  (/ (55, I = 1, foo2%l1)/)

          TYPE IS (Child(4,*,4))
             foo2%base_arr  = (/ (550, I = 1, foo2%l1)/)
             foo2%child_arr = (/ (-550, I = 1, foo2%l1)/)

          TYPE IS (Base(4,*))
             foo2%base_arr  = (/ (5550, I = 1, foo2%l1)/)

          CLASS DEFAULT
           STOP 111
      END SELECT

      END FUNCTION foo2
!*
      PROGRAM Generic_Interface01e
      USE MOD1
      IMPLICIT NONE

      INTEGER :: I

      CLASS(Base(4,:)), POINTER :: poly1
      TYPE(Child(4,5,4)), TARGET :: tgt1
      CLASS(Base(4,:)), POINTER  :: base1

      CLASS(Base(4,:)), POINTER :: poly2(:,:)
      TYPE(Child(4,5,4)), TARGET :: tgt2(2,2)
      CLASS(Base(4,:)), POINTER  :: base2(:,:)

      ALLOCATE(base(4,5) :: base1)

      SELECT TYPE ( A => FUNC(base1) )
          TYPE IS (Base(4,*))
            IF( size(A%base_arr)  .NE. 5) ERROR STOP 10
            IF( ANY(A%base_arr .NE. 1000 )) ERROR STOP 11

          CLASS DEFAULT
           STOP 12
      END SELECT

      poly1 => FUNC(base1)

      SELECT TYPE ( A => FUNC(poly1) )
          TYPE IS (Base(4,*))
            IF( size(A%base_arr)  .NE. 5) ERROR STOP 13
            IF( ANY(A%base_arr .NE. 1000 )) ERROR STOP 14

          CLASS DEFAULT
           STOP 15
      END SELECT

      ALLOCATE(Base(4,10):: poly1)

      SELECT TYPE ( A => FUNC(poly1) )
          TYPE IS (Base(4,*))
            IF( size(A%base_arr)  .NE. 10) ERROR STOP 16
            IF( ANY(A%base_arr .NE. 1000 )) ERROR STOP 17

          CLASS DEFAULT
           STOP 18
      END SELECT

      poly1 => tgt1

      SELECT TYPE ( A => FUNC(poly1) )
          TYPE IS (Child(4,*,4))
            IF( size(A%base_arr)  .NE. 5) ERROR STOP 19
            IF( ANY(A%base_arr .NE. 100 )) ERROR STOP 20
            IF( size(A%child_arr)  .NE. 5) ERROR STOP 21
            IF( ANY(A%child_arr .NE. -100 )) ERROR STOP 22

          CLASS DEFAULT
           STOP 23
      END SELECT

      ALLOCATE(NextGen(4,10,4,4):: poly1)

      SELECT TYPE ( A => FUNC(poly1) )
          TYPE IS (NextGen(4,*,4,4))
            IF( size(A%base_arr)  .NE. 10) ERROR STOP 24
            IF( ANY(A%base_arr .NE. 1 )) ERROR STOP 25
            IF( size(A%child_arr)  .NE. 10) ERROR STOP 26
            IF( ANY(A%child_arr .NE. -10 )) ERROR STOP 27
            IF( size(A%nextg_arr)  .NE. 10) ERROR STOP 28
            IF( ANY(A%nextg_arr .NE. 10 )) ERROR STOP 29

          CLASS DEFAULT
           STOP 30
      END SELECT

      ALLOCATE(base(4,5) :: base2(10,10))

      SELECT TYPE ( A => FUNC(base2) )
          TYPE IS (Base(4,*))
            IF( size(A%base_arr)  .NE. 5) ERROR STOP 31
            IF( ANY(A%base_arr .NE. 5550 )) ERROR STOP 32

          CLASS DEFAULT
           STOP 33
      END SELECT

      ALLOCATE(Base(4,10):: poly2(1,1))

      SELECT TYPE ( A => FUNC(poly2) )
          TYPE IS (Base(4,*))
            IF( size(A%base_arr)  .NE. 10) ERROR STOP 34
            IF( ANY(A%base_arr .NE. 5550 )) ERROR STOP 35

          CLASS DEFAULT
           STOP 36
      END SELECT

      poly2 => tgt2

      SELECT TYPE ( A => FUNC(poly2) )
          TYPE IS (Child(4,*,4))
            IF( size(A%base_arr)  .NE. 5) ERROR STOP 37
            IF( ANY(A%base_arr .NE. 550 )) ERROR STOP 38
            IF( size(A%child_arr)  .NE. 5) ERROR STOP 39
            IF( ANY(A%child_arr .NE. -550 )) ERROR STOP 40

          CLASS DEFAULT
           STOP 41
      END SELECT

      ALLOCATE(NextGen(4,10,4,4):: poly2(2,2))

      SELECT TYPE ( A => FUNC(poly2) )
          TYPE IS (NextGen(4,*,4,4))
            IF( size(A%base_arr)  .NE. 10) ERROR STOP 42
            IF( ANY(A%base_arr .NE. 5 )) ERROR STOP 43
            IF( size(A%child_arr)  .NE. 10) ERROR STOP 44
            IF( ANY(A%child_arr .NE. -55 )) ERROR STOP 45
            IF( size(A%nextg_arr)  .NE. 10) ERROR STOP 46
            IF( ANY(A%nextg_arr .NE. 55 )) ERROR STOP 47

          CLASS DEFAULT
           STOP 48
      END SELECT

      END PROGRAM Generic_Interface01e
