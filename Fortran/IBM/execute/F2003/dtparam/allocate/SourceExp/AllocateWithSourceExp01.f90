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
!* Defect 361702
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
       TYPE(NextGen(4,10,4,10,5,5)), ALLOCATABLE :: n1(:)
       CLASS(Base(4,4)), POINTER :: bptr

       CHARACTER(100) :: string
       INTEGER :: I

       ALLOCATE (Base(4,10) :: b1(2))
       IF (SIZE(b1) .NE. 2) STOP 10
       IF (LEN(b1(1)%Calloc) .NE. 10) STOP 11
       IF (LEN(b1(2)%Calloc) .NE. 10) STOP 12
       IF (ANY(b1(1)%Calloc .NE. 'AAA')) STOP 13
       IF (ANY(b1(2)%Calloc .NE. 'AAA')) STOP 14

       string = 'Niels'

! k3=5 is possible because k3 not used to define any intrinsic variable

       ALLOCATE(n1(5), source = NextGen(4,10,4,10,5,5) (Calloc = 'XLFtest', Rarr = 9.5, &
                 & next = c1, Carr = string, Iarr = 1))

       IF (SIZE(n1) .NE. 5) STOP 15
       DO I = 1, 5
          IF (LEN(n1(I)%Calloc) .NE. 10) STOP 16
          IF (ANY(n1(I)%Calloc .NE. 'XLFtest')) STOP 17
          IF (SIZE(n1(I)%Rarr) .NE. 10) STOP 18
          IF (ANY(n1(I)%Rarr .NE. 9.5)) STOP 19
          IF (LEN(n1(I)%Carr) .NE. 10) STOP 20
          IF (ANY(n1(I)%Carr .NE. string)) STOP 21
          IF (SIZE(n1(I)%Iarr) .NE. 10) STOP 22
          IF (ANY(n1(I)%Iarr .NE. 1)) STOP 23
       END DO

       call sub(n1)

       IF (SIZE(n1) .NE. 2) STOP 24
       IF (LEN(n1(1)%Calloc) .NE. 10) STOP 25
       IF (ANY(n1(1)%Calloc .NE. 'BBB')) STOP 26
       IF (SIZE(n1(1)%Rarr) .NE. 10) STOP 27
       IF (ANY(n1(1)%Rarr .NE. 66.22)) STOP 28
       IF (LEN(n1(1)%Carr) .NE. 10) STOP 29
       IF (ANY(n1(1)%Carr .NE. 'Erwin')) STOP 30
       IF (SIZE(n1(1)%Iarr) .NE. 10) STOP 31
       IF (ANY(n1(1)%Iarr .NE. 5)) STOP 32

       IF (LEN(n1(2)%Calloc) .NE. 10) STOP 34
       IF (ANY(n1(2)%Calloc .NE. 'CCC')) STOP 35
       IF (SIZE(n1(2)%Rarr) .NE. 10) STOP 36
       IF (ANY(n1(2)%Rarr .NE. 22.66)) STOP 37
       IF (LEN(n1(2)%Carr) .NE. 10) STOP 38
       IF (ANY(n1(2)%Carr .NE. 'Werne')) STOP 39
       IF (SIZE(n1(2)%Iarr) .NE. 10) STOP 40
       IF (ANY(n1(2)%Iarr .NE. 7)) STOP 41

       ALLOCATE (bptr, source = Child(4,4,4,4) ('XLFt', -2))

       SELECT TYPE(bptr)
          TYPE IS(Child(4,*,4,*))
             ALLOCATE(bptr%next,source=bptr)
             IF (LEN(bptr%Calloc) .NE. 4) STOP 42
             IF (ANY(bptr%Calloc .NE. 'XLFt')) STOP 43
             IF (SIZE(bptr%Rarr) .NE. 4) STOP 44
             IF (ANY(bptr%Rarr .NE. -2)) STOP 45
             SELECT TYPE( A => bptr%next)
                TYPE IS(Child(4,*,4,*))
                   IF (LEN(A%Calloc) .NE. 4) STOP 46
                   IF (ANY(A%Calloc .NE. 'XLFt')) STOP 47
                   IF (SIZE(A%Rarr) .NE. 4) STOP 48
                   IF (ANY(A%Rarr .NE. -2)) STOP 49

                CLASS DEFAULT
                   STOP 50
             END SELECT
          CLASS DEFAULT
             STOP 51
       END SELECT

       DEALLOCATE(b1,n1,bptr)

      CONTAINS
!*
      SUBROUTINE sub(argn1)
       TYPE(NextGen(4,*,4,*,5,*)), ALLOCATABLE, INTENT(OUT) :: argn1(:)
       TYPE(Base(4,10)), TARGET :: tgt

       ALLOCATE(argn1(2), SOURCE = [NextGen(4,10,4,10,5,5) ('BBB', 66.22, tgt, 'Erwin', 5),  &
              & NextGen(4,10,4,10,5,5) ('CCC', 22.66, tgt, 'Werne', 7)])

      END SUBROUTINE sub

END PROGRAM AllocateWithSourceExp01
