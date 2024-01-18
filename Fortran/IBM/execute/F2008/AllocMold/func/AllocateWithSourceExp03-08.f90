!* ===================================================================
!*
!* DATE                       : June 2, 2015
!* ORIGIN                     : AIX Compiler Development,
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
!* TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/SourceExp/AllocateWithSourceExp03.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM AllocateWithSourceExp03
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1 = KIND(0)
        INTEGER, LEN  :: l1 = 10

        CHARACTER(l1)  :: Carr(l1) = 'ABCD '
        INTEGER(k1) :: Iarr(l1) = 1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = KIND(0)
        INTEGER, LEN  :: l2 = 10

        TYPE(Base(k2,l2)) :: Cmp
      END TYPE Child

      TYPE(Base(4,2)) :: b1 = Base(4,2) ('Werner', 1)
      TYPE(Child(4,2,4,2)), POINTER :: c1, c2

      ALLOCATE(c1, c2, SOURCE = func(b1))

      IF (ANY(c1%Carr .NE. 'TE')) ERROR STOP 30
      IF (ANY(c1%Iarr .NE. 99)) ERROR STOP 31
      IF (ANY(c1%Cmp%Carr .NE. 'We')) ERROR STOP 32
      IF (ANY(c1%Cmp%Iarr .NE. 1)) ERROR STOP 33

      IF (ANY(c2%Carr .NE. 'TE')) ERROR STOP 10
      IF (ANY(c2%Iarr .NE. 99)) ERROR STOP 11
      IF (ANY(c2%Cmp%Carr .NE. 'We')) ERROR STOP 12
      IF (ANY(c2%Cmp%Iarr .NE. 1)) ERROR STOP 13

      c2 => func(b1)
      IF (ANY(c1%Carr .NE. 'TE')) ERROR STOP 34
      IF (ANY(c1%Iarr .NE. 99)) ERROR STOP 35
      IF (ANY(c1%Cmp%Carr .NE. 'We')) ERROR STOP 36
      IF (ANY(c1%Cmp%Iarr .NE. 1)) ERROR STOP 37

      IF (ANY(c2%Carr .NE. 'TE')) ERROR STOP 14
      IF (ANY(c2%Iarr .NE. 99)) ERROR STOP 15
      IF (ANY(c2%Cmp%Carr .NE. 'We')) ERROR STOP 16
      IF (ANY(c2%Cmp%Iarr .NE. 1)) ERROR STOP 17

      ALLOCATE(c1, c2, SOURCE = foo(b1))
      IF (ANY(c1%Carr .NE. 'TE')) ERROR STOP 38
      IF (ANY(c1%Iarr .NE. 99)) ERROR STOP 39
      IF (ANY(c1%Cmp%Carr .NE. 'We')) ERROR STOP 40
      IF (ANY(c1%Cmp%Iarr .NE. 1)) ERROR STOP 41

      IF (ANY(c2%Carr .NE. 'TE')) ERROR STOP 18
      IF (ANY(c2%Iarr .NE. 99)) ERROR STOP 19
      IF (ANY(c2%Cmp%Carr .NE. 'We')) ERROR STOP 20
      IF (ANY(c2%Cmp%Iarr .NE. 1)) ERROR STOP 21

      c2 => foo(b1)
      IF (ANY(c1%Carr .NE. 'TE')) ERROR STOP 42
      IF (ANY(c1%Iarr .NE. 99)) ERROR STOP 43
      IF (ANY(c1%Cmp%Carr .NE. 'We')) ERROR STOP 44
      IF (ANY(c1%Cmp%Iarr .NE. 1)) ERROR STOP 45

      IF (ANY(c2%Carr .NE. 'TE')) ERROR STOP 22
      IF (ANY(c2%Iarr .NE. 99)) ERROR STOP 23
      IF (ANY(c2%Cmp%Carr .NE. 'We')) ERROR STOP 24
      IF (ANY(c2%Cmp%Iarr .NE. 1)) ERROR STOP 25

      CONTAINS
!*
      FUNCTION func(Arg)
        TYPE(Base(4,*)) :: Arg
        TYPE(Child(4,:,4,:)), POINTER :: func

        ALLOCATE(func, SOURCE = Child(4,Arg%l1,4,Arg%l1) (Carr = 'TEST', &
              & Cmp = Arg, Iarr = 99) )
      END FUNCTION func

      FUNCTION foo(Arg)
        CLASS(Base(4,*)) :: Arg
        TYPE(Child(4,:,4,:)), POINTER :: foo

        ALLOCATE(foo, SOURCE = Child(4,Arg%l1,4,Arg%l1) (Carr = 'TEST', &
              & Cmp = Arg, Iarr = 99) )
      END FUNCTION foo
END PROGRAM AllocateWithSourceExp03
