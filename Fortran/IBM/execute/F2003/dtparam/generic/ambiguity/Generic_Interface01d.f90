!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Resolution for polymorphic objects
!*                               based on the number of arguments
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

      TYPE, EXTENDS(Base) :: Child (k2,k3,l2,l3)
        INTEGER, KIND :: k2, k3
        INTEGER, LEN  :: l2, l3

        INTEGER(k2) :: child_arr(l1+l2)
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4

        INTEGER(k1+k3) :: nextg_arr(l1+l3)
      END TYPE NextGen

      INTERFACE FUNC
        CLASS(Base(4,:)) FUNCTION foo1(Obj)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Base(4,*)) :: Obj
           ALLOCATABLE :: foo1
         END FUNCTION foo1

         CLASS(Child(4,:,4,4,:,:)) FUNCTION foo2(Obj,Arg)
           IMPORT BASE, CHILD, NEXTGEN
           CLASS(Child(4,*,4,4,*,*)) :: Obj
           CLASS(Base(4,*)) :: Arg
           ALLOCATABLE :: foo2
         END FUNCTION foo2
      END INTERFACE

      END MODULE Mod1
!*
      CLASS(Base(4,:)) FUNCTION foo1(Obj)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Base(4,*)) :: Obj
        ALLOCATABLE :: foo1
        INTEGER :: I , K

        ALLOCATE (foo1, source = Obj)
        IF ( .NOT. ALLOCATED(foo1)) ERROR STOP 100

        SELECT TYPE ( foo1 )
          TYPE IS (NextGen(4,*,4,4,*,*,4,*))
             foo1%base_arr  = (/ (1, I = 1, foo1%l3)/)
             K = foo1%l1 + foo1%l2
             foo1%child_arr = (/ (-10, I = 1, K)/)
             K = foo1%l1 + foo1%l4
             foo1%nextg_arr =  (/ (10, I = 1, K)/)

          TYPE IS (Child(4,*,4,4,*,*))
             foo1%base_arr  = (/ (100, I = 1, foo1%l2)/)
             K = foo1%l1 + foo1%l2
             foo1%child_arr = (/ (-100, I = 1, K)/)

          TYPE IS (Base(4,*))
             foo1%base_arr  = (/ (1000, I = 1, foo1%l1)/)

          CLASS DEFAULT
           STOP 110
      END SELECT

      END FUNCTION foo1

      CLASS(Child(4,:,4,4,:,:)) FUNCTION foo2(Obj,Arg)
        USE MOD1, Only: Base, Child, NextGen
        CLASS(Child(4,*,4,4,*,*)) :: Obj
        CLASS(Base(4,*)) :: Arg
        ALLOCATABLE :: foo2
        INTEGER :: I , K

        ALLOCATE (foo2, source = Obj)
        IF ( .NOT. ALLOCATED(foo2)) ERROR STOP 101

        SELECT TYPE ( foo2 )
          TYPE IS (NextGen(4,*,4,4,*,*,4,*))
             foo2%base_arr  = (/ (5, I = 1, foo2%l4)/)
             K = foo2%l1 + foo2%l2
             foo2%child_arr = (/ (-55, I = 1, K)/)
             K = foo2%l1 + foo2%l4
             foo2%nextg_arr =  (/ (55, I = 1, K)/)

          TYPE IS (Child(4,*,4,4,*,*))
             foo2%base_arr  = (/ (550, I = 1, foo2%l3)/)
             K = foo2%l1 + foo2%l2
             foo2%child_arr = (/ (-550, I = 1, K)/)

          CLASS DEFAULT
           STOP 111
      END SELECT

      END FUNCTION foo2
!*
      PROGRAM Generic_Interface01d
      USE MOD1
      IMPLICIT NONE

      CLASS(Base(4,:)), POINTER :: poly1
      TYPE(Child(4,5,4,4,1,1)), TARGET :: tgt1
      TYPE(Base(4,5))  :: base1
      TYPE(child(4,10,4,4,100,100)) :: child1
      TYPE(NextGen(4,10,4,4,1,1,4,1)) :: dtv

      SELECT TYPE ( A => FUNC(base1) )
          TYPE IS (Base(4,*))
            IF( size(A%base_arr)  .NE. A%l1 ) ERROR STOP 10
            IF( ANY(A%base_arr .NE. 1000 )) ERROR STOP 11

          CLASS DEFAULT
           STOP 12
      END SELECT

      ALLOCATE(Base(4,10):: poly1)

      SELECT TYPE ( A => FUNC(poly1) )   ! dynamic type BASE call foo1
          TYPE IS (Base(4,*))
            IF( size(A%base_arr)  .NE. 10) ERROR STOP 13
            IF( ANY(A%base_arr .NE. 1000 )) ERROR STOP 14

          CLASS DEFAULT
           STOP 15
      END SELECT

      poly1 => tgt1

      SELECT TYPE ( A => FUNC(poly1) )     ! dynamic type Child call foo1
          TYPE IS (Child(4,*,4,4,*,*))
            IF( size(A%base_arr)  .NE. A%l2) ERROR STOP 16
            IF( ANY(A%base_arr .NE. 100 )) ERROR STOP 17
            IF( size(A%child_arr)  .NE. (A%l1+A%l2) ) ERROR STOP 18
            IF( ANY(A%child_arr .NE. -100 )) ERROR STOP 19

          CLASS DEFAULT
           STOP 20
      END SELECT

      ALLOCATE(NextGen(4,10,4,4,1,1,4,1):: poly1)

      SELECT TYPE ( A => FUNC(poly1) )    ! dynamic type NextGen call foo1
          TYPE IS (NextGen(4,*,4,4,*,*,4,*))
            IF( size(A%base_arr)  .NE. A%l3) ERROR STOP 21
            IF( ANY(A%base_arr .NE. 1 )) ERROR STOP 22
            IF( size(A%child_arr)  .NE. (A%l1+A%l2)) ERROR STOP 23
            IF( ANY(A%child_arr .NE. -10 )) ERROR STOP 24
            IF( size(A%nextg_arr)  .NE. (A%l1+A%l4)) ERROR STOP 25
            IF( ANY(A%nextg_arr .NE. 10 )) ERROR STOP 26

          CLASS DEFAULT
           STOP 27
      END SELECT

      SELECT TYPE ( A => poly1)
          CLASSIS (NextGen(4,*,4,4,*,*,4,*))
             SELECT TYPE ( B => FUNC(A,A) )
                TYPE IS (NextGen(4,*,4,4,*,*,4,*))
                  IF( size(B%base_arr)  .NE. B%l4) ERROR STOP 28
                  IF( ANY(B%base_arr .NE. 5 )) ERROR STOP 29
                  IF( size(B%child_arr)  .NE. (B%l1+B%l2)) ERROR STOP 30
                  IF( ANY(B%child_arr .NE. -55 )) ERROR STOP 31
                  IF( size(B%nextg_arr)  .NE. (B%l1+B%l4)) ERROR STOP 32
                  IF( ANY(B%nextg_arr .NE. 55 )) ERROR STOP 33

                CLASS DEFAULT
                   STOP 34
                END SELECT
          CLASS DEFAULT
           STOP 35
      END SELECT

      END PROGRAM Generic_Interface01d
