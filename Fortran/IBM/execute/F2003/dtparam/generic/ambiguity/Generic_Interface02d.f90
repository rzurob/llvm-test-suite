!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects
!*                                 based on the number of arguments
!*                                 and type incompatibility
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

      TYPE, EXTENDS(Base) :: Child1 (k2)
        INTEGER, KIND :: k2

        INTEGER(k2), ALLOCATABLE :: child_arr(:)
      END TYPE Child1

      TYPE, EXTENDS(Base) :: Child2
        INTEGER(k1), ALLOCATABLE :: child_arr(:)
      END TYPE Child2

      TYPE, EXTENDS(Child1) :: NextGen(k3)
        INTEGER, KIND :: k3

        INTEGER(k3), ALLOCATABLE :: nextg_arr(:)
      END TYPE NextGen

      INTERFACE SUB
         SUBROUTINE sub0(pntr,Obj)
           IMPORT BASE, CHILD1, CHILD2, NEXTGEN
           CLASS(Base(4,*)) :: Obj
           CLASS(Base(4,:)), ALLOCATABLE, INTENT(OUT) :: pntr
         END SUBROUTINE

         SUBROUTINE sub1(pntr, n)
           IMPORT CHILD1
           CLASS(Child1(4,:,4)), ALLOCATABLE, INTENT(OUT) :: pntr
           integer, intent(in) :: n
         END SUBROUTINE

         SUBROUTINE sub2(pntr, n)
           IMPORT CHILD2
           CLASS(Child2(4,:)), ALLOCATABLE, INTENT(OUT) :: pntr
           integer, intent(in) :: n
         END SUBROUTINE
      END INTERFACE

      END MODULE Mod1
!*
      PROGRAM Generic_Interface02d
      USE MOD1
      IMPLICIT NONE

      TYPE(Base(4,5))  :: b1
      TYPE(Child1(4,5,4)), TARGET :: c1
      TYPE(Child2(4,10)) :: c2
      TYPE(NextGen(4,10,4,4)) :: n1

      CLASS(Base(4,:)), ALLOCATABLE :: poly_b1
      CLASS(Child1(4,:,4)), ALLOCATABLE :: poly_c1
      CLASS(Child2(4,:)), ALLOCATABLE :: poly_c2

      CALL sub(poly_b1,b1)                                ! 2 arguments call sub0
      IF ( .NOT. ALLOCATED(poly_b1)) STOP 10

      IF( size(poly_b1%base_arr) .NE. 5) STOP 11
      IF( ANY(poly_b1%base_arr .NE. 1000 )) STOP 12

      DEALLOCATE(poly_b1)
      CALL sub(poly_b1,c1)                                ! 2 arguments call sub0
      IF ( .NOT. ALLOCATED(poly_b1)) STOP 13

        SELECT TYPE ( poly_b1 )
          TYPE IS (Child1(4,*,4))
            IF( size(poly_b1%base_arr) .NE. 5) STOP 14
            IF( ANY(poly_b1%base_arr .NE. 400  )) STOP 15
            IF( size(poly_b1%child_arr)  .NE. 5) STOP 16
            IF( ANY(poly_b1%child_arr .NE. -400 )) STOP 17

          CLASS DEFAULT
           STOP 18
      END SELECT

      DEALLOCATE(poly_b1)
      CALL sub(poly_b1,c2)                                ! 2 arguments call sub0
      IF ( .NOT. ALLOCATED(poly_b1)) STOP 19

        SELECT TYPE ( poly_b1 )
          TYPE IS (Child2(4,*))
            IF( size(poly_b1%base_arr) .NE. 10) STOP 20
            IF( ANY(poly_b1%base_arr .NE. 100 )) STOP 21
            IF( size(poly_b1%child_arr)  .NE. 10) STOP 22
            IF( ANY(poly_b1%child_arr .NE. -100 )) STOP 23

          CLASS DEFAULT
           STOP 24
      END SELECT

      DEALLOCATE(poly_b1)
      CALL sub(poly_b1,n1)                                ! 2 arguments call sub0
      IF ( .NOT. ALLOCATED(poly_b1)) STOP 25

        SELECT TYPE ( poly_b1 )
          TYPE IS (NextGen(4,*,4,4))
            IF( size(poly_b1%base_arr) .NE. 10) STOP 26
            IF( ANY(poly_b1%base_arr .NE. 4 )) STOP 27
            IF( size(poly_b1%child_arr)  .NE. 10) STOP 28
            IF( ANY(poly_b1%child_arr .NE. -40 )) STOP 29
            IF( size(poly_b1%nextg_arr)  .NE. 10) STOP 30
            IF( ANY(poly_b1%nextg_arr .NE. 40 )) STOP 31

          CLASS DEFAULT
           STOP 32
      END SELECT

      DEALLOCATE(poly_b1)

      ALLOCATE(NextGen(4,100,4,4):: poly_c1)

      CALL sub(poly_c1, 100)                                   ! 1 argument, dynamic type NextGen: call sub1
      IF ( .NOT. ALLOCATED(poly_c1)) STOP 33

        SELECT TYPE ( poly_c1 )                           ! dynamic type child1 after call to sub1
          TYPE IS (Child1(4,*,4))
            IF( size(poly_c1%base_arr) .NE. 9) STOP 34
            IF( ANY(poly_c1%base_arr .NE. 55 )) STOP 35
            IF( size(poly_c1%child_arr) .NE. 9) STOP 36
            IF( ANY(poly_c1%child_arr .NE. -55 )) STOP 37

          CLASS DEFAULT
           STOP 38
      END SELECT

      DEALLOCATE(poly_c1)

      ALLOCATE(poly_c1, source=c1)

      CALL sub(poly_c1, 5)                                   ! 1 argument, dynamic type Child1: call sub1
      IF ( .NOT. ALLOCATED(poly_c1)) STOP 39

        SELECT TYPE ( poly_c1 )                           ! dynamic type child1 after call to sub1
          TYPE IS (Child1(4,*,4))
            IF( size(poly_c1%base_arr) .NE. 9) STOP 40
            IF( ANY(poly_c1%base_arr .NE. 55 )) STOP 41
            IF( size(poly_c1%child_arr) .NE. 9) STOP 42
            IF( ANY(poly_c1%child_arr .NE. -55 )) STOP 43

          CLASS DEFAULT
           STOP 44
      END SELECT

      DEALLOCATE(poly_c1)

      ALLOCATE(Child2(4,1):: poly_c2)

      CALL sub(poly_c2, 1)                                   ! 1 argument, dynamic type Child2: call sub2
      IF ( .NOT. ALLOCATED(poly_c2)) STOP 45

        SELECT TYPE ( poly_c2 )                           ! dynamic type child2 after call to sub1
          TYPE IS (Child2(4,*))
            IF( size(poly_c2%base_arr) .NE. 11) STOP 46
            IF( ANY(poly_c2%base_arr .NE. 33 )) STOP 47
            IF( size(poly_c2%child_arr) .NE. 11) STOP 48
            IF( ANY(poly_c2%child_arr .NE. -33 )) STOP 49

          CLASS DEFAULT
           STOP 50
      END SELECT

      END PROGRAM Generic_Interface02d
!*
      SUBROUTINE sub0(pntr,Obj)
        USE MOD1, Only: Base, Child1, child2, NextGen
        CLASS(Base(4,*)) :: Obj
        CLASS(Base(4,:)), ALLOCATABLE, INTENT(OUT) :: pntr

        ALLOCATE (pntr, source = Obj)
        IF ( .NOT. ALLOCATED(pntr)) STOP 100

        SELECT TYPE ( pntr )
          TYPE IS (NextGen(4,*,4,4))
             pntr%base_arr  = (/ (4, I = 1, pntr%l1)/)
             pntr%child_arr = (/ (-40, I = 1, pntr%l1)/)
             pntr%nextg_arr = (/ (40, I = 1, pntr%l1)/)

          TYPE IS (NextGen(4,*,8,4))
             pntr%base_arr  = (/ (84, I = 1, pntr%l1)/)
             pntr%child_arr = (/ (-84, I = 1, pntr%l1)/)
             pntr%nextg_arr = (/ (84, I = 1, pntr%l1)/)

          TYPE IS (NextGen(4,*,4,8))
             pntr%base_arr  = (/ (48, I = 1, pntr%l1)/)
             pntr%child_arr = (/ (-48, I = 1, pntr%l1)/)
             pntr%nextg_arr = (/ (48, I = 1, pntr%l1)/)

          TYPE IS (NextGen(4,*,8,8))
             pntr%base_arr  = (/ (88, I = 1, pntr%l1)/)
             pntr%child_arr = (/ (-88, I = 1, pntr%l1)/)
             pntr%nextg_arr = (/ (88, I = 1, pntr%l1)/)

          TYPE IS (Child1(4,*,4))
             pntr%base_arr  = (/ (400, I = 1, pntr%l1)/)
             pntr%child_arr = (/ (-400, I = 1, pntr%l1)/)

          TYPE IS (Child1(4,*,8))
             pntr%base_arr  = (/ (800, I = 1, pntr%l1)/)
             pntr%child_arr = (/ (-800, I = 1, pntr%l1)/)

          TYPE IS (Child2(4,*))
             pntr%base_arr  = (/ (100, I = 1, pntr%l1)/)
             pntr%child_arr = (/ (-100, I = 1, pntr%l1)/)

          TYPE IS (Base(4,*))
             pntr%base_arr  = (/ (1000, I = 1, pntr%l1)/)

          CLASS DEFAULT
           STOP 110
      END SELECT

      END SUBROUTINE

      SUBROUTINE sub1(pntr, n)
        USE MOD1, Only: Child1
        CLASS(Child1(4,:,4)), ALLOCATABLE, INTENT(OUT) :: pntr
        integer, intent(in) :: n

        ALLOCATE (Child1(4,n,4) :: pntr)
        IF ( .NOT. ALLOCATED(pntr)) STOP 101

        pntr%base_arr  = (/ (55, I = 1, 9 )/)
        pntr%child_arr = (/ (-55, I = 1, 9 )/)

      END SUBROUTINE
!*
      SUBROUTINE sub2(pntr, n)
        USE MOD1, Only: Child2
        CLASS(Child2(4,:)), ALLOCATABLE, INTENT(OUT) :: pntr
        integer, intent(in) :: n

        ALLOCATE (Child2(4,n) :: pntr)
        IF ( .NOT. ALLOCATED(pntr)) STOP 102

        pntr%base_arr  = (/ (33, I = 1, 11  )/)
        pntr%child_arr = (/ (-33, I = 1, 11 )/)

      END SUBROUTINE
