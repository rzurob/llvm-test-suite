!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression - Host Association
!*  SECONDARY FUNCTIONS TESTED : Specification expression in Function Result
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  An objet designator with a base object that is made accessible by host association
!*
!*  Defect 359556
!*
!234567890123456789012345678901234567890123456789012345678901234567890
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

         INTEGER(k2) :: A2(l2)
         CHARACTER(l2) :: C2
         CLASS(Base(k2,l2)), POINTER :: cmp1
      END TYPE
END MODULE
PROGRAM SpecExpHostAssociation06
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, J
      TYPE(Base(4,5)) :: b1
      TYPE(Child(4,1,4,10)) :: c1
      CLASS(Base(4,:)), POINTER :: ptr

      b1 = Base(4,5)(5, [1,2,3,4,5], 'AAAAA')
      ptr => foo11(b1)
      IF ( ptr%k1 .NE. b1%k1 ) ERROR STOP 20
      IF ( ptr%l1 .NE. b1%l1 ) ERROR STOP 21
      IF ( ptr%I1 .NE. b1%I1 ) ERROR STOP 21
      IF ( ANY(ptr%A1   .NE.      b1%A1) ) ERROR STOP 21
      IF ( TRIM(ptr%C1) .NE. TRIM(b1%C1) ) ERROR STOP 21

      ptr => foo12(b1)
      SELECT TYPE ( ptr )
         TYPE IS (Child(4,*,4,*))
           IF ( ptr%k1 .NE. b1%k1 ) ERROR STOP 20
           IF ( ptr%l1 .NE. b1%l1 ) ERROR STOP 21
           IF ( ptr%k2 .NE. b1%k1 ) ERROR STOP 20
           IF ( ptr%l2 .NE. b1%l1 ) ERROR STOP 21
           IF ( ptr%I1 .NE. b1%I1 ) ERROR STOP 21
           IF ( ANY(ptr%A1   .NE.      b1%A1) ) ERROR STOP 21
           IF ( ANY(ptr%A2   .NE.    2*b1%A1) ) ERROR STOP 21
           IF ( TRIM(ptr%C1) .NE. TRIM(b1%C1) ) ERROR STOP 21
           IF ( TRIM(ptr%C2) .NE. TRIM(b1%C1) ) ERROR STOP 21

        CLASS DEFAULT
           STOP 17
      END SELECT


      c1 = Child(4,1,4,10)( 8, [88], 'X', [(99*I, I = 1,10)], 'XLFtest', NULL() )
      ALLOCATE( c1%cmp1, SOURCE = Base(4,10)( 3, [(7*I, I = 1,10)], 'IBM' ) )
      ptr => foo21(c1)
      ptr => foo22(c1)

      CONTAINS

      FUNCTION foo11(this) Result(Res)
        CLASS(Base(4,*)) :: this
        TYPE(Base(b1%k1,b1%l1)), POINTER :: Res

        ALLOCATE( Res )
        IF ( Res%k1 .NE. this%k1 ) ERROR STOP 10
        IF ( Res%l1 .NE. this%l1  ) ERROR STOP 11
        IF ( SIZE(Res%A1) .NE.this%l1  ) ERROR STOP 12
        IF ( LEN(Res%C1)  .NE. this%l1 ) ERROR STOP 13

        Res%I1 = this%I1
        Res%A1 = this%A1
        Res%C1 = this%C1
      END FUNCTION foo11

      FUNCTION foo12(this) Result(Res)
        CLASS(Base(4,*)) :: this
        TYPE(Child(KIND(b1%I1),UBOUND(b1%A1,1),b1%k1,b1%l1)), POINTER :: Res

        ALLOCATE( Res )
        IF ( Res%k1 .NE. KIND(this%I1) ) ERROR STOP 14
        IF ( Res%l1 .NE. SIZE(this%A1) ) ERROR STOP 15
        IF ( Res%k2 .NE.       this%k1 ) ERROR STOP 16
        IF ( Res%l2 .NE.       this%l1 ) ERROR STOP 17
        IF ( SIZE(Res%A1) .NE. SIZE(this%A1) ) ERROR STOP 18
        IF ( LEN(Res%C1)  .NE. SIZE(this%A1) ) ERROR STOP 19
        IF ( SIZE(Res%A2) .NE.       this%l1 ) ERROR STOP 20
        IF ( LEN(Res%C2)  .NE.       this%l1 ) ERROR STOP 21

        Res%I1 = this%I1
        Res%A1 = this%A1
        Res%A2 = 2*this%A1
        Res%C1 = this%C1
        Res%C2 = this%C1

        ALLOCATE( Res%cmp1, SOURCE = this )
        IF ( .NOT. ASSOCIATED(Res%cmp1) ) ERROR STOP 22

        IF ( Res%cmp1%k1 .NE. this%k1 ) ERROR STOP 23
        IF ( Res%cmp1%l1 .NE. this%l1 ) ERROR STOP 24
        IF ( SIZE(Res%cmp1%A1) .NE. this%l1 ) ERROR STOP 25
        IF ( LEN(Res%cmp1%C1)  .NE. this%l1 ) ERROR STOP 26
      END FUNCTION foo12

      FUNCTION foo21(this) Result(Res)
        CLASS(Child(4,*,4,*)) :: this
        TYPE(Base(c1%k1,c1%l1)), POINTER :: Res

        ALLOCATE( Res )
        IF ( Res%k1 .NE. this%k1 ) ERROR STOP 30
        IF ( Res%l1 .NE. this%l1 ) ERROR STOP 31
        IF ( SIZE(Res%A1) .NE.this%l1  ) ERROR STOP 32
        IF ( LEN(Res%C1)  .NE. this%l1 ) ERROR STOP 33

        Res%I1 = this%I1
        Res%A1 = this%A1
        Res%C1 = this%C1
      END FUNCTION foo21

      FUNCTION foo22(this) Result(Res)
        CLASS(Child(4,*,4,*)) :: this
        TYPE(Child(c1%k1,c1%l1,c1%cmp1%k1,c1%cmp1%l1)), POINTER :: Res

        ALLOCATE( Res )
        IF ( Res%k1 .NE. this%k1 ) ERROR STOP 34
        IF ( Res%l1 .NE. this%l1 ) ERROR STOP 35
        IF ( Res%k2 .NE. this%k2 ) ERROR STOP 36
        IF ( Res%l2 .NE. this%l2 ) ERROR STOP 37
        IF ( SIZE(Res%A1) .NE. this%l1 ) ERROR STOP 38
        IF ( LEN(Res%C1)  .NE. this%l1 ) ERROR STOP 39
        IF ( SIZE(Res%A2) .NE. this%l2 ) ERROR STOP 40
        IF ( LEN(Res%C2)  .NE. this%l2 ) ERROR STOP 41

        Res%I1 = this%I1
        Res%A1 = this%A1
        Res%A2 = this%A2
        Res%C1 = this%C1
        Res%C2 = this%C2

        ALLOCATE( Res%cmp1, SOURCE = this%cmp1 )
        IF ( .NOT. ASSOCIATED(Res%cmp1) ) ERROR STOP 42
        IF ( Res%cmp1%k1 .NE. this%cmp1%k1 ) ERROR STOP 43
        IF ( Res%cmp1%l1 .NE. this%cmp1%l1 ) ERROR STOP 44
        IF ( SIZE(Res%cmp1%A1) .NE. this%cmp1%l1 ) ERROR STOP 45
        IF ( LEN(Res%cmp1%C1)  .NE. this%cmp1%l1 ) ERROR STOP 46
      END FUNCTION foo22
END PROGRAM SpecExpHostAssociation06
