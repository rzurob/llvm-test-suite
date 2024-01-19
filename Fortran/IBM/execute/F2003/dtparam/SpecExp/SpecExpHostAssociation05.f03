!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression - Host Association
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
!*  An objet designator with a base object that is made accessible by host association
!*  Polymorphic base object
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
         TYPE(Base(k2,l2)) :: cmp1
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
         INTEGER, KIND :: k3
         INTEGER, LEN  :: l3

         INTEGER(k3) :: A3(l3)
         CHARACTER(l3) :: C3
         TYPE(Base(k3,l3)) :: cmp2
      END TYPE
END MODULE
PROGRAM SpecExpHostAssociation05
      USE Mod
      IMPLICIT NONE

      INTEGER :: I, J
      CLASS(Base(4,:)), POINTER :: b1
      CLASS(Child(4,:,4,:)), POINTER :: c1
      CLASS(NextGen(4,:,4,:,4,:)), POINTER :: n1

      ALLOCATE( b1, SOURCE = Base(4,5)(5, [1,2,3,4,5], 'AAAAA') )
      IF ( .NOT. ASSOCIATED(b1) ) STOP 06
      CALL Sub11(b1)
      CALL Sub12(b1)
      CALL Sub13(b1)
      CALL Sub14(b1)

      ALLOCATE( b1, SOURCE = Base(4,7)(I1 = 10, A1 = [(10*I, I = 1,7)], C1 = 'BBBBBBB') )
      IF ( .NOT. ASSOCIATED(b1) ) STOP 07
      CALL Sub11(b1)
      CALL Sub12(b1)
      CALL Sub13(b1)
      CALL Sub14(b1)

      ALLOCATE( c1, SOURCE = Child(4,1,4,10) ( 8, [88], 'X', [(99*I, I = 1,10)], 'XLFtest',  &
                             Base(4,10)(3, [(7*I, I = 1,10)], 'IBM') ) )
      IF ( .NOT. ASSOCIATED(c1) ) STOP 08
      CALL Sub21(c1)
      CALL Sub22(c1)
      CALL Sub23(c1)
      CALL Sub24(c1)

      ALLOCATE( n1, SOURCE = NextGen(4,100,4,50,4,25) ( I1 = 8, A1 = [(I, I = 1,100)], C1 = 'Base',   &
                                                                A2 = [(I, I = 1,50)], C2 = 'Child',   &
                                                              A3 = [(I, I = 1,25)], C3 = 'NextGen',   &
                                                   cmp1 = Base(4,50)(3, [(7*I, I = 1,50)], 'Base1'),  &
                                                   cmp2 = Base(4,25)(3, [(7*I, I = 1,25)], 'Base2') ) )
      IF ( .NOT. ASSOCIATED(n1) ) STOP 08
      CALL Sub31(n1)
      CALL Sub32(n1)
      CALL Sub33(n1)
      CALL Sub34(n1)

      CONTAINS

      SUBROUTINE Sub11(this)
        CLASS(Base(4,*)) :: this
        CLASS(Base(b1%k1,b1%l1)), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE. this%k1 ) ERROR STOP 10
        IF ( Obj%l1 .NE. this%l1 ) ERROR STOP 11
        IF ( SIZE(Obj%A1) .NE. this%l1 ) ERROR STOP 12
        IF ( LEN(Obj%C1)  .NE. this%l1 ) ERROR STOP 13
      END SUBROUTINE Sub11

      SUBROUTINE Sub21(this)
        CLASS(Child(4,*,4,*)) :: this
        CLASS(Base(c1%cmp1%k1,c1%cmp1%l1)), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE. this%cmp1%k1 ) ERROR STOP 14
        IF ( Obj%l1 .NE. this%cmp1%l1 ) ERROR STOP 15
        IF ( SIZE(Obj%A1) .NE. this%cmp1%l1 ) ERROR STOP 16
        IF ( LEN(Obj%C1)  .NE. this%cmp1%l1 ) ERROR STOP 17
      END SUBROUTINE Sub21

      SUBROUTINE Sub31(this)
        CLASS(NextGen(4,*,4,*,4,*)) :: this
        CLASS(Base(n1%cmp2%k1,n1%cmp2%l1)), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE. this%cmp2%k1 ) ERROR STOP 18
        IF ( Obj%l1 .NE. this%cmp2%l1 ) ERROR STOP 19
        IF ( SIZE(Obj%A1) .NE. this%cmp2%l1 ) ERROR STOP 20
        IF ( LEN(Obj%C1)  .NE. this%cmp2%l1 ) ERROR STOP 21
      END SUBROUTINE Sub31

      SUBROUTINE Sub12(this)
        CLASS(Base(4,*)) :: this
        CLASS(Child(KIND(b1%I1),b1%I1,2*KIND(b1%I1),2*SIZE(b1%A1))), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE.   KIND(this%I1) ) ERROR STOP 22
        IF ( Obj%l1 .NE.         this%I1 ) ERROR STOP 23
        IF ( Obj%k2 .NE. 2*KIND(this%I1) ) ERROR STOP 24
        IF ( Obj%l2 .NE. 2*SIZE(this%A1) ) ERROR STOP 25
        IF ( SIZE(Obj%A1) .NE.         this%I1 ) ERROR STOP 26
        IF ( LEN(Obj%C1)  .NE.         this%I1 ) ERROR STOP 27
        IF ( SIZE(Obj%A2) .NE. 2*SIZE(this%A1) ) ERROR STOP 28
        IF ( LEN(Obj%C2)  .NE. 2*SIZE(this%A1) ) ERROR STOP 29

        IF ( Obj%cmp1%k1 .NE. 2*KIND(this%I1) ) ERROR STOP 30
        IF ( Obj%cmp1%l1 .NE. 2*SIZE(this%A1) ) ERROR STOP 31
        IF ( SIZE(Obj%cmp1%A1) .NE. 2*SIZE(this%A1) ) ERROR STOP 32
        IF ( LEN(Obj%cmp1%C1)  .NE. 2*SIZE(this%A1) ) ERROR STOP 33
      END SUBROUTINE Sub12

      SUBROUTINE Sub22(this)
        CLASS(Child(4,*,4,*)) :: this
        CLASS(Child(KIND(c1%cmp1%I1),c1%cmp1%I1,2*KIND(c1%cmp1%I1),2*SIZE(c1%cmp1%A1))), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE.   KIND(this%cmp1%I1) ) ERROR STOP 34
        IF ( Obj%l1 .NE.         this%cmp1%I1 ) ERROR STOP 35
        IF ( Obj%k2 .NE. 2*KIND(this%cmp1%I1) ) ERROR STOP 36
        IF ( Obj%l2 .NE. 2*SIZE(this%cmp1%A1) ) ERROR STOP 37
        IF ( SIZE(Obj%A1) .NE.         this%cmp1%I1 ) ERROR STOP 38
        IF ( LEN(Obj%C1)  .NE.         this%cmp1%I1 ) ERROR STOP 39
        IF ( SIZE(Obj%A2) .NE. 2*SIZE(this%cmp1%A1) ) ERROR STOP 40
        IF ( LEN(Obj%C2)  .NE. 2*SIZE(this%cmp1%A1) ) ERROR STOP 41

        IF ( Obj%cmp1%k1 .NE. 2*KIND(this%cmp1%I1) ) ERROR STOP 42
        IF ( Obj%cmp1%l1 .NE. 2*SIZE(this%cmp1%A1) ) ERROR STOP 43
        IF ( SIZE(Obj%cmp1%A1) .NE. 2*SIZE(this%cmp1%A1) ) ERROR STOP 44
        IF ( LEN(Obj%cmp1%C1)  .NE. 2*SIZE(this%cmp1%A1) ) ERROR STOP 45
      END SUBROUTINE Sub22

      SUBROUTINE Sub32(this)
        CLASS(NextGen(4,*,4,*,4,*)) :: this
        CLASS(Child(KIND(n1%cmp2%I1),n1%cmp2%I1,2*KIND(n1%cmp2%I1),2*SIZE(n1%cmp2%A1))), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE.   KIND(this%cmp2%I1) ) ERROR STOP 46
        IF ( Obj%l1 .NE.         this%cmp2%I1 ) ERROR STOP 47
        IF ( Obj%k2 .NE. 2*KIND(this%cmp2%I1) ) ERROR STOP 48
        IF ( Obj%l2 .NE. 2*SIZE(this%cmp2%A1) ) ERROR STOP 49
        IF ( SIZE(Obj%A1) .NE.         this%cmp2%I1 ) ERROR STOP 50
        IF ( LEN(Obj%C1)  .NE.         this%cmp2%I1 ) ERROR STOP 51
        IF ( SIZE(Obj%A2) .NE. 2*SIZE(this%cmp2%A1) ) ERROR STOP 52
        IF ( LEN(Obj%C2)  .NE. 2*SIZE(this%cmp2%A1) ) ERROR STOP 53

        IF ( Obj%cmp1%k1 .NE. 2*KIND(this%cmp2%I1) ) ERROR STOP 54
        IF ( Obj%cmp1%l1 .NE. 2*SIZE(this%cmp2%A1) ) ERROR STOP 55
        IF ( SIZE(Obj%cmp1%A1) .NE. 2*SIZE(this%cmp2%A1) ) ERROR STOP 56
        IF ( LEN(Obj%cmp1%C1)  .NE. 2*SIZE(this%cmp2%A1) ) ERROR STOP 57
      END SUBROUTINE Sub32

      SUBROUTINE Sub13(this)
        CLASS(Base(4,*)) :: this
        CLASS(NextGen(KIND(b1%A1(1)),b1%A1(1),KIND(b1%A1(2)),b1%A1(2),KIND(b1%A1(3)),b1%A1(3))), &
                                                                                  ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE. KIND(this%A1(1)) ) ERROR STOP 58
        IF ( Obj%l1 .NE.       this%A1(1) ) ERROR STOP 59
        IF ( Obj%k2 .NE. KIND(this%A1(2)) ) ERROR STOP 60
        IF ( Obj%l2 .NE.       this%A1(2) ) ERROR STOP 61
        IF ( Obj%k3 .NE. KIND(this%A1(3)) ) ERROR STOP 62
        IF ( Obj%l3 .NE.       this%A1(3) ) ERROR STOP 63
        IF ( SIZE(Obj%A1) .NE. this%A1(1) ) ERROR STOP 64
        IF ( LEN(Obj%C1)  .NE. this%A1(1) ) ERROR STOP 65
        IF ( SIZE(Obj%A2) .NE. this%A1(2) ) ERROR STOP 66
        IF ( LEN(Obj%C2)  .NE. this%A1(2) ) ERROR STOP 67
        IF ( SIZE(Obj%A3) .NE. this%A1(3) ) ERROR STOP 68
        IF ( LEN(Obj%C3)  .NE. this%A1(3) ) ERROR STOP 69

        IF ( Obj%cmp1%k1 .NE. KIND(this%A1(2)) ) ERROR STOP 70
        IF ( Obj%cmp1%l1 .NE.       this%A1(2) ) ERROR STOP 71
        IF ( SIZE(Obj%cmp1%A1) .NE. this%A1(2) ) ERROR STOP 72
        IF ( LEN(Obj%cmp1%C1)  .NE. this%A1(2) ) ERROR STOP 73

        IF ( Obj%cmp2%k1 .NE. KIND(this%A1(3)) ) ERROR STOP 74
        IF ( Obj%cmp2%l1 .NE.       this%A1(3) ) ERROR STOP 75
        IF ( SIZE(Obj%cmp2%A1) .NE. this%A1(3) ) ERROR STOP 76
        IF ( LEN(Obj%cmp2%C1)  .NE. this%A1(3) ) ERROR STOP 77
      END SUBROUTINE Sub13

      SUBROUTINE Sub23(this)
        CLASS(Child(4,*,4,*)) :: this
        CLASS(NextGen(KIND(c1%cmp1%A1(1)),c1%cmp1%A1(1),KIND(c1%cmp1%A1(2)),c1%cmp1%A1(2),  &
                                        KIND(c1%cmp1%A1(3)),c1%cmp1%A1(3))), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE. KIND(this%cmp1%A1(1)) ) ERROR STOP 78
        IF ( Obj%l1 .NE.       this%cmp1%A1(1) ) ERROR STOP 79
        IF ( Obj%k2 .NE. KIND(this%cmp1%A1(2)) ) ERROR STOP 80
        IF ( Obj%l2 .NE.       this%cmp1%A1(2) ) ERROR STOP 81
        IF ( Obj%k3 .NE. KIND(this%cmp1%A1(3)) ) ERROR STOP 82
        IF ( Obj%l3 .NE.       this%cmp1%A1(3) ) ERROR STOP 83
        IF ( SIZE(Obj%A1) .NE. this%cmp1%A1(1) ) ERROR STOP 84
        IF ( LEN(Obj%C1)  .NE. this%cmp1%A1(1) ) ERROR STOP 85
        IF ( SIZE(Obj%A2) .NE. this%cmp1%A1(2) ) ERROR STOP 86
        IF ( LEN(Obj%C2)  .NE. this%cmp1%A1(2) ) ERROR STOP 87
        IF ( SIZE(Obj%A3) .NE. this%cmp1%A1(3) ) ERROR STOP 88
        IF ( LEN(Obj%C3)  .NE. this%cmp1%A1(3) ) ERROR STOP 89

        IF ( Obj%cmp1%k1 .NE. KIND(this%cmp1%A1(2)) ) ERROR STOP 90
        IF ( Obj%cmp1%l1 .NE.       this%cmp1%A1(2) ) ERROR STOP 91
        IF ( SIZE(Obj%cmp1%A1) .NE. this%cmp1%A1(2) ) ERROR STOP 92
        IF ( LEN(Obj%cmp1%C1)  .NE. this%cmp1%A1(2) ) ERROR STOP 93

        IF ( Obj%cmp2%k1 .NE. KIND(this%cmp1%A1(3)) ) ERROR STOP 94
        IF ( Obj%cmp2%l1 .NE.       this%cmp1%A1(3) ) ERROR STOP 95
        IF ( SIZE(Obj%cmp2%A1) .NE. this%cmp1%A1(3) ) ERROR STOP 96
        IF ( LEN(Obj%cmp2%C1)  .NE. this%cmp1%A1(3) ) ERROR STOP 97
      END SUBROUTINE Sub23

      SUBROUTINE Sub33(this)
        CLASS(NextGen(4,*,4,*,4,*)) :: this
        CLASS(NextGen(KIND(n1%cmp2%A1(1)),n1%cmp2%A1(1),KIND(n1%cmp2%A1(2)),n1%cmp2%A1(2),  &
                                        KIND(n1%cmp2%A1(3)),n1%cmp2%A1(3))), ALLOCATABLE :: Obj

        ALLOCATE( Obj )
        IF ( Obj%k1 .NE. KIND(this%cmp2%A1(1)) ) ERROR STOP 98
        IF ( Obj%l1 .NE.       this%cmp2%A1(1) ) ERROR STOP 99
        IF ( Obj%k2 .NE. KIND(this%cmp2%A1(2)) ) ERROR STOP 100
        IF ( Obj%l2 .NE.       this%cmp2%A1(2) ) ERROR STOP 101
        IF ( Obj%k3 .NE. KIND(this%cmp2%A1(3)) ) ERROR STOP 102
        IF ( Obj%l3 .NE.       this%cmp2%A1(3) ) ERROR STOP 103
        IF ( SIZE(Obj%A1) .NE. this%cmp2%A1(1) ) ERROR STOP 104
        IF ( LEN(Obj%C1)  .NE. this%cmp2%A1(1) ) ERROR STOP 105
        IF ( SIZE(Obj%A2) .NE. this%cmp2%A1(2) ) ERROR STOP 106
        IF ( LEN(Obj%C2)  .NE. this%cmp2%A1(2) ) ERROR STOP 107
        IF ( SIZE(Obj%A3) .NE. this%cmp2%A1(3) ) ERROR STOP 108
        IF ( LEN(Obj%C3)  .NE. this%cmp2%A1(3) ) ERROR STOP 109

        IF ( Obj%cmp1%k1 .NE. KIND(this%cmp2%A1(2)) ) ERROR STOP 110
        IF ( Obj%cmp1%l1 .NE.       this%cmp2%A1(2) ) ERROR STOP 111
        IF ( SIZE(Obj%cmp1%A1) .NE. this%cmp2%A1(2) ) ERROR STOP 112
        IF ( LEN(Obj%cmp1%C1)  .NE. this%cmp2%A1(2) ) ERROR STOP 113

        IF ( Obj%cmp2%k1 .NE. KIND(this%cmp2%A1(3)) ) ERROR STOP 114
        IF ( Obj%cmp2%l1 .NE.       this%cmp2%A1(3) ) ERROR STOP 115
        IF ( SIZE(Obj%cmp2%A1) .NE. this%cmp2%A1(3) ) ERROR STOP 116
        IF ( LEN(Obj%cmp2%C1)  .NE. this%cmp2%A1(3) ) ERROR STOP 117
      END SUBROUTINE Sub33

      SUBROUTINE Sub14(this)
        INTEGER :: I
        CLASS(Base(4,*)) :: this
        TYPE(Base(KIND(b1%I1),LEN(b1%C1))) :: Obj(b1%I1)

        IF ( SIZE(Obj) .NE. this%I1 ) ERROR STOP 118
        DO I = 1, this%I1
           IF ( Obj(I)%k1 .NE. KIND(this%I1) ) ERROR STOP 119
           IF ( Obj(I)%l1 .NE.  LEN(this%C1) ) ERROR STOP 120
           IF ( SIZE(Obj(I)%A1) .NE. LEN(this%C1) ) ERROR STOP 121
           IF ( LEN(Obj(I)%C1)  .NE. LEN(this%C1) ) ERROR STOP 122
        END DO
      END SUBROUTINE Sub14

      SUBROUTINE Sub24(this)
        INTEGER :: I
        CLASS(Child(4,*,4,*)) :: this
        TYPE(Base(KIND(c1%cmp1%I1),LEN(c1%cmp1%C1))) :: Obj(c1%cmp1%I1)

        IF ( SIZE(Obj) .NE. this%cmp1%I1 ) ERROR STOP 123
        DO I = 1, this%cmp1%I1
           IF ( Obj(I)%k1 .NE. KIND(this%cmp1%I1) ) ERROR STOP 124
           IF ( Obj(I)%l1 .NE.  LEN(this%cmp1%C1) ) ERROR STOP 125
           IF ( SIZE(Obj(I)%A1) .NE. LEN(this%cmp1%C1) ) ERROR STOP 126
           IF ( LEN(Obj(I)%C1)  .NE. LEN(this%cmp1%C1) ) ERROR STOP 127
        END DO
      END SUBROUTINE Sub24

      SUBROUTINE Sub34(this)
        INTEGER :: I
        CLASS(NextGen(4,*,4,*,4,*)) :: this
        TYPE(Base(KIND(n1%cmp2%I1),LEN(n1%cmp2%C1))) :: Obj(n1%cmp2%I1)

        IF ( SIZE(Obj) .NE. this%cmp2%I1 ) STOP 01
        DO I = 1, this%cmp2%I1
           IF ( Obj(I)%k1 .NE. KIND(this%cmp2%I1) ) STOP 02
           IF ( Obj(I)%l1 .NE.  LEN(this%cmp2%C1) ) STOP 03
           IF ( SIZE(Obj(I)%A1) .NE. LEN(this%cmp2%C1) ) STOP 04
           IF ( LEN(Obj(I)%C1)  .NE. LEN(this%cmp2%C1) ) STOP 05
        END DO
      END SUBROUTINE Sub34
END PROGRAM SpecExpHostAssociation05
