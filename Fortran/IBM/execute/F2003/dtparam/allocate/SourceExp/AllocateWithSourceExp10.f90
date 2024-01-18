!*  ===================================================================
!*
!*  DATE                       : January 20, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with source expression
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: Calloc = 'AAA'
        INTEGER(k1)   :: Iarr(l1) = k1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        CLASS(Base(k2,l1)), POINTER :: next => NULL()
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3

        CHARACTER(l2) :: Carr(l2) = 'ABCD '
        INTEGER(k2)   :: Imat(l2,l1) = k3
        REAL(k2+k1)   :: Rarr(l1) = k2
      END TYPE NextGen
END MODULE Mod1
PROGRAM AllocateWithSourceExp10
      USE MOD1
      IMPLICIT NONE

       TYPE(Base(4,20)), POINTER :: b1
       TYPE(Base(4,:)), ALLOCATABLE :: b2
       CLASS(Base(4,:)), POINTER :: b3
       CLASS(*), POINTER :: upoly

       ALLOCATE (b1, SOURCE = Base(4,20)('XLFtest', 100))

       b1 = shuffle (b1, b2)
       IF (b1%Calloc .NE. 'XLFtest') ERROR STOP 10
       IF ( ANY(b1%Iarr .NE. 100) )  ERROR STOP 11
       IF (b2%Calloc .NE. 'Shuffle') ERROR STOP 12
       IF ( ANY(b2%Iarr .NE. -99) )  ERROR STOP 13

       b1 => shuffle (b1, b2)
       IF (b1%Calloc .NE. 'XLFtest') ERROR STOP 14
       IF ( ANY(b1%Iarr .NE. 100) )  ERROR STOP 15
       IF (b2%Calloc .NE. 'Shuffle') ERROR STOP 16
       IF ( ANY(b2%Iarr .NE. -99) )  ERROR STOP 17

       ALLOCATE (b3, SOURCE = shuffle (b1, b2) )
       IF (b3%Calloc .NE. 'XLFtest') ERROR STOP 18
       IF ( ANY(b3%Iarr .NE. 100) )  ERROR STOP 19
       IF (b1%Calloc .NE. 'XLFtest') ERROR STOP 20
       IF ( ANY(b1%Iarr .NE. 100) )  ERROR STOP 21
       IF (b2%Calloc .NE. 'Shuffle') ERROR STOP 22
       IF ( ANY(b2%Iarr .NE. -99) )  ERROR STOP 23

       ALLOCATE (upoly, SOURCE = shuffle (b1, b2) )
       IF (b1%Calloc .NE. 'XLFtest') ERROR STOP 24
       IF ( ANY(b1%Iarr .NE. 100) )  ERROR STOP 25
       IF (b2%Calloc .NE. 'Shuffle') ERROR STOP 26
       IF ( ANY(b2%Iarr .NE. -99) )  ERROR STOP 27

       SELECT TYPE ( upoly )
          TYPEIS (Base(4,*))
            IF (upoly%Calloc .NE. 'XLFtest') ERROR STOP 28
            IF ( ANY(upoly%Iarr .NE. 100) )  ERROR STOP 29

          CLASSDEFAULT
            STOP 30
       END SELECT


       DEALLOCATE(b1,b2,b3,upoly)

      CONTAINS
!*
       TYPE(Base(4,:)) FUNCTION shuffle(a, b)
         TYPE(Base(4,*)), INTENT(IN), POINTER :: a
         TYPE(Base(4,:)), INTENT(OUT), ALLOCATABLE  :: b
         POINTER :: shuffle

         ALLOCATE (b, SOURCE = Base(4,20)('Shuffle', -99))

         ALLOCATE (shuffle , SOURCE = a)
       END FUNCTION

END PROGRAM AllocateWithSourceExp10
