!*  ===================================================================
!*
!*  DATE                       : May 13, 2015
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
!*  TEST CASE ADAPTED FROM     : $(tsrcdir)/F2003/dtparam/allocate/SourceExp/AllocateWithSourceExp01.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: Calloc(l1) = 'AAA'
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        REAL :: Rarr(l1) = k2
        CLASS(Base(k2,l1)), POINTER :: next => NULL()
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3

        CHARACTER(l2) :: Carr(l2) = 'ABCD '
        INTEGER(k2)   :: Iarr(l2) = k3
      END TYPE NextGen
END MODULE Mod1

PROGRAM AllocateWithSourceExp01
      USE MOD1
      IMPLICIT NONE

      TYPE(Base(4,:)), POINTER :: b1(:)
      TYPE(Child(4,10,4,10)), TARGET :: c1 = Child(4,10,4,10)(Rarr = -1)
      TYPE(NextGen(4,10,4,10,5,5)), ALLOCATABLE :: n1(:), n2(:)
      CLASS(Base(4,4)), POINTER :: bptr1, bptr2

      CHARACTER(100) :: string
      INTEGER :: I

      ALLOCATE (Base(4,10) :: b1(2))
      IF (SIZE(b1) .NE. 2) ERROR STOP 10
      IF (LEN(b1(1)%Calloc) .NE. 10) ERROR STOP 11
      IF (LEN(b1(2)%Calloc) .NE. 10) ERROR STOP 12
      IF (ANY(b1(1)%Calloc .NE. 'AAA')) ERROR STOP 13
      IF (ANY(b1(2)%Calloc .NE. 'AAA')) ERROR STOP 14

      string = 'Niels'

! k3=5 is possible because k3 not used to define any intrinsic variable

      ALLOCATE(n1(5), n2(5), source = NextGen(4,10,4,10,5,5) (Calloc = 'XLFtest', Rarr = 9.5, &
                & next = c1, Carr = string, Iarr = 1))


!      Allocate array n1 with 5 elements of this source
      IF (SIZE(n1) .NE. 5) ERROR STOP 150
      IF (SIZE(n2) .NE. 5) ERROR STOP 151

   print *, "DONE 1"
      DO I = 1, 5

!     Check n1
          IF (LEN(n1(I)%Calloc) .NE. 10) ERROR STOP 16
          IF (ANY(n1(I)%Calloc .NE. 'XLFtest')) ERROR STOP 17
          IF (SIZE(n1(I)%Rarr) .NE. 10) ERROR STOP 18
          IF (ANY(n1(I)%Rarr .NE. 9.5)) ERROR STOP 19
          IF (LEN(n1(I)%Carr) .NE. 10) ERROR STOP 20
          IF (ANY(n1(I)%Carr .NE. string)) ERROR STOP 21
          IF (SIZE(n1(I)%Iarr) .NE. 10) ERROR STOP 22
          IF (ANY(n1(I)%Iarr .NE. 1)) ERROR STOP 23

!     Check n2
          IF (LEN(n2(I)%Calloc) .NE. 10) ERROR STOP 52
          IF (ANY(n2(I)%Calloc .NE. 'XLFtest')) ERROR STOP 53
          IF (SIZE(n2(I)%Rarr) .NE. 10) ERROR STOP 54
          IF (ANY(n2(I)%Rarr .NE. 9.5)) ERROR STOP 55
          IF (LEN(n2(I)%Carr) .NE. 10) ERROR STOP 56
          IF (ANY(n2(I)%Carr .NE. string)) ERROR STOP 57
          IF (SIZE(n2(I)%Iarr) .NE. 10) ERROR STOP 58
          IF (ANY(n2(I)%Iarr .NE. 1)) ERROR STOP 59
      END DO

       call sub(n1, n2)

!     Check n1
       IF (SIZE(n1) .NE. 2) ERROR STOP 24
       IF (LEN(n1(1)%Calloc) .NE. 10) ERROR STOP 25
       IF (ANY(n1(1)%Calloc .NE. 'BBB')) ERROR STOP 26
       IF (SIZE(n1(1)%Rarr) .NE. 10) ERROR STOP 27
       IF (ANY(n1(1)%Rarr .NE. 66.22)) ERROR STOP 28
       IF (LEN(n1(1)%Carr) .NE. 10) ERROR STOP 29
       IF (ANY(n1(1)%Carr .NE. 'Erwin')) ERROR STOP 30
       IF (SIZE(n1(1)%Iarr) .NE. 10) ERROR STOP 31
       IF (ANY(n1(1)%Iarr .NE. 5)) ERROR STOP 32

       IF (LEN(n1(2)%Calloc) .NE. 10) ERROR STOP 34
       IF (ANY(n1(2)%Calloc .NE. 'CCC')) ERROR STOP 35
       IF (SIZE(n1(2)%Rarr) .NE. 10) ERROR STOP 36
       IF (ANY(n1(2)%Rarr .NE. 22.66)) ERROR STOP 37
       IF (LEN(n1(2)%Carr) .NE. 10) ERROR STOP 38
       IF (ANY(n1(2)%Carr .NE. 'Werne')) ERROR STOP 39
       IF (SIZE(n1(2)%Iarr) .NE. 10) ERROR STOP 40
       IF (ANY(n1(2)%Iarr .NE. 7)) ERROR STOP 41

!     Check n2
       IF (SIZE(n2) .NE. 2) ERROR STOP 60
       IF (LEN(n2(1)%Calloc) .NE. 10) ERROR STOP 61
       IF (ANY(n2(1)%Calloc .NE. 'BBB')) ERROR STOP 62
       IF (SIZE(n2(1)%Rarr) .NE. 10) ERROR STOP 63
       IF (ANY(n2(1)%Rarr .NE. 66.22)) ERROR STOP 64
       IF (LEN(n2(1)%Carr) .NE. 10) ERROR STOP 65
       IF (ANY(n2(1)%Carr .NE. 'Erwin')) ERROR STOP 66
       IF (SIZE(n2(1)%Iarr) .NE. 10) ERROR STOP 67
       IF (ANY(n2(1)%Iarr .NE. 5)) ERROR STOP 68

       IF (LEN(n2(2)%Calloc) .NE. 10) ERROR STOP 69
       IF (ANY(n2(2)%Calloc .NE. 'CCC')) ERROR STOP 70
       IF (SIZE(n2(2)%Rarr) .NE. 10) ERROR STOP 71
       IF (ANY(n2(2)%Rarr .NE. 22.66)) ERROR STOP 72
       IF (LEN(n2(2)%Carr) .NE. 10) ERROR STOP 73
       IF (ANY(n2(2)%Carr .NE. 'Werne')) ERROR STOP 74
       IF (SIZE(n2(2)%Iarr) .NE. 10) ERROR STOP 75
       IF (ANY(n2(2)%Iarr .NE. 7)) ERROR STOP 76


       ALLOCATE (bptr1, bptr2, source = Child(4,4,4,4) ('XLFt', -2))

       SELECT TYPE(bptr1)
          TYPE IS(Child(4,*,4,*))
             ALLOCATE(bptr1%next,source=bptr1)
             IF (LEN(bptr1%Calloc) .NE. 4) ERROR STOP 42
             IF (ANY(bptr1%Calloc .NE. 'XLFt')) ERROR STOP 43
             IF (SIZE(bptr1%Rarr) .NE. 4) ERROR STOP 44
             IF (ANY(bptr1%Rarr .NE. -2)) ERROR STOP 45
             SELECT TYPE( A => bptr1%next)
                TYPE IS(Child(4,*,4,*))
                   IF (LEN(A%Calloc) .NE. 4) ERROR STOP 46
                   IF (ANY(A%Calloc .NE. 'XLFt')) ERROR STOP 47
                   IF (SIZE(A%Rarr) .NE. 4) ERROR STOP 48
                   IF (ANY(A%Rarr .NE. -2)) ERROR STOP 49

                CLASS DEFAULT
                   STOP 50
             END SELECT
          CLASS DEFAULT
             STOP 51
       END SELECT

       SELECT TYPE(bptr2)
          TYPE IS(Child(4,*,4,*))
             ALLOCATE(bptr2%next,source=bptr2)
             IF (LEN(bptr2%Calloc) .NE. 4) ERROR STOP 142
             IF (ANY(bptr2%Calloc .NE. 'XLFt')) ERROR STOP 143
             IF (SIZE(bptr2%Rarr) .NE. 4) ERROR STOP 144
             IF (ANY(bptr2%Rarr .NE. -2)) ERROR STOP 145
             SELECT TYPE( A => bptr2%next)
                TYPE IS(Child(4,*,4,*))
                   IF (LEN(A%Calloc) .NE. 4) ERROR STOP 146
                   IF (ANY(A%Calloc .NE. 'XLFt')) ERROR STOP 147
                   IF (SIZE(A%Rarr) .NE. 4) ERROR STOP 148
                   IF (ANY(A%Rarr .NE. -2)) ERROR STOP 149

                CLASS DEFAULT
                   STOP 150
             END SELECT
          CLASS DEFAULT
             STOP 151
       END SELECT


       print *, "DONE 2"

       DEALLOCATE(b1,n1,n2,bptr1,bptr2)
       print *, "DONE 3"

       CONTAINS
!*
       SUBROUTINE sub(argn1, argn2)
       TYPE(NextGen(4,*,4,*,5,*)), ALLOCATABLE, INTENT(OUT) :: argn1(:), argn2(:)

       TYPE(Base(4,10)), TARGET :: tgt
       print *, "SUBROUTINE"
       ALLOCATE(argn1(2), argn2(2), SOURCE = [NextGen(4,10,4,10,5,5) ('BBB', 66.22, tgt, 'Erwin', 5),  &
              & NextGen(4,10,4,10,5,5) ('CCC', 22.66, tgt, 'Werne', 7)])

       END SUBROUTINE sub

END PROGRAM AllocateWithSourceExp01