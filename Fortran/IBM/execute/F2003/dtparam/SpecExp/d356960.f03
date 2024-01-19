MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1
         INTEGER, LEN  :: l1

         INTEGER(k1) :: I1, A1(l1)
         CHARACTER(l1) :: C1
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2
         INTEGER, LEN  :: l2

         CLASS(Base(k2,l2)), ALLOCATABLE :: ptr
      END TYPE
END MODULE
      USE Mod
      IMPLICIT NONE

      INTEGER I
      TYPE(Child(4,10,4,5)) :: c1 = Child(4,10,4,5) &
                            ( 10, 20 , 'Heisenberg', NULL() )

      ALLOCATE( c1%ptr, SOURCE = Base(4,5) ( 30, [(I, I =1,5)], 'ABCDE' ) )
      CALL Sub13(4,30)

      CONTAINS

      SUBROUTINE Sub13(N,M)
        INTEGER :: N, M
        TYPE(Child(KIND(c1%ptr%I1),c1%ptr%I1,KIND(c1%ptr%I1),2*c1%ptr%I1)) :: Obj

         IF ( Obj%k1 .NE. N ) ERROR STOP 24
         IF ( Obj%l1 .NE. M ) ERROR STOP 25
         IF ( Obj%k2 .NE. N ) ERROR STOP 26
         IF ( Obj%l2 .NE. 2*M ) ERROR STOP 27
         IF ( SIZE(Obj%A1) .NE. M ) ERROR STOP 28
         IF ( LEN(Obj%C1)  .NE. M ) ERROR STOP 29

         print*, KIND(c1%ptr%I1),2*c1%ptr%I1
         print*, Obj%k2,Obj%l2
         ALLOCATE( Base(Obj%k2,Obj%l2) :: Obj%ptr )
         print*, Obj%ptr%k1, Obj%ptr%l1
         IF ( Obj%ptr%k1 .NE.   N ) ERROR STOP 30
         IF ( Obj%ptr%l1 .NE. 2*M ) ERROR STOP 31
         IF ( SIZE(Obj%ptr%A1) .NE. 2*M ) ERROR STOP 32
         IF ( LEN(Obj%ptr%C1)  .NE. 2*M ) ERROR STOP 33
      END SUBROUTINE Sub13
END
