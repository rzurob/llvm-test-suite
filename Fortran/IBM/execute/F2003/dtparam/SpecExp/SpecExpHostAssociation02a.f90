!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Specification expression - Host Association
!*  SECONDARY FUNCTIONS TESTED : Default Initialization
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  An object designator with a base object that is made accessible by host association
!*
!234567890123456789012345678901234567890123456789012345678901234567890
PROGRAM SpecExpHostAssociation02a
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 4
         INTEGER, LEN  :: l1 = 1

         INTEGER(k1) :: I1 = k1, A1(l1) = 2*k1
         CHARACTER(l1) :: C1 = 'B'
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
         INTEGER, KIND :: k2 = 8
         INTEGER, LEN  :: l2 = 1

         TYPE(Base(k2,k2)) :: b_cmp = Base(k2,k2) ( k1+k2, 2*(k1+k2), CHAR(100-(k1+k2)) )
      END TYPE

      TYPE(Child) :: c1
      TYPE(Child(l1=3,l2=5)) :: c2

      CALL Sub11(8)
      CALL Sub12(8)
      CALL Sub13(12)
      CALL Sub14(24)
      CALL Sub15(8)
      CALL Sub16(8)

      c1 = Child ( b_cmp = Base(8,8) (I1 = 10, A1 = 20) )
      CALL Sub11(8)
      CALL Sub12(8)
      CALL Sub13(10)
      CALL Sub14(20)
      CALL Sub15(8)
      CALL Sub16(8)

      CALL Sub21(8)
      CALL Sub22(8)
      CALL Sub23(12)
      CALL Sub24(24)
      CALL Sub25(8)
      CALL Sub26(8)

      c2 = Child(4,3,8,5) ( b_cmp = Base(8,8) (I1 = 11, A1 = 22) )
      CALL Sub21(8)
      CALL Sub22(8)
      CALL Sub23(11)
      CALL Sub24(22)
      CALL Sub25(8)
      CALL Sub26(8)

      CONTAINS

      SUBROUTINE Sub11(N)
        INTEGER :: N
        TYPE(Base(c1%b_cmp%k1,c1%b_cmp%l1)) :: Obj

         IF ( Obj%k1 .NE. N ) STOP 10
         IF ( Obj%l1 .NE. N ) STOP 11
         IF ( Obj%I1 .NE. N ) STOP 12
         IF ( SIZE(Obj%A1) .NE.    N ) STOP 13
         IF ( ANY(Obj%A1   .NE. 2*N) ) STOP 14
         IF ( LEN(Obj%C1)  .NE.    N ) STOP 15
         IF ( TRIM(Obj%C1) .NE.  'B' ) STOP 16
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N)
        INTEGER :: N
        TYPE(Base(c1%b_cmp%l1,2*c1%b_cmp%k1)) :: Obj

         IF ( Obj%k1 .NE.   N ) STOP 17
         IF ( Obj%l1 .NE. 2*N ) STOP 18
         IF ( Obj%I1 .NE.   N ) STOP 19
         IF ( SIZE(Obj%A1) .NE.  2*N ) STOP 20
         IF ( ANY(Obj%A1   .NE. 2*N) ) STOP 21
         IF ( LEN(Obj%C1)  .NE.  2*N ) STOP 22
         IF ( TRIM(Obj%C1) .NE.  'B' ) STOP 23
      END SUBROUTINE Sub12

      SUBROUTINE Sub13(N)
        INTEGER :: N
        TYPE(Base(l1=c1%b_cmp%I1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 24
         IF ( Obj%l1 .NE. N ) STOP 25
         IF ( Obj%I1 .NE. 4 ) STOP 26
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 27
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 28
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 29
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 30
      END SUBROUTINE Sub13

      SUBROUTINE Sub14(N)
        INTEGER :: N
        TYPE(Base(l1=c1%b_cmp%A1(1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 31
         IF ( Obj%l1 .NE. N ) STOP 32
         IF ( Obj%I1 .NE. 4 ) STOP 33
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 34
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 35
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 36
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 37
      END SUBROUTINE Sub14

      SUBROUTINE Sub15(N)
        INTEGER :: N
        TYPE(Base(KIND(c1%b_cmp%I1),KIND(c1%b_cmp%I1))) :: Obj

         IF ( Obj%k1 .NE. N ) STOP 38
         IF ( Obj%l1 .NE. N ) STOP 39
         IF ( Obj%I1 .NE. N ) STOP 40
         IF ( SIZE(Obj%A1) .NE.    N ) STOP 41
         IF ( ANY(Obj%A1   .NE. 2*N) ) STOP 42
         IF ( LEN(Obj%C1)  .NE.    N ) STOP 43
         IF ( TRIM(Obj%C1) .NE.  'B' ) STOP 44
      END SUBROUTINE Sub15

      SUBROUTINE Sub16(N)
        INTEGER :: N
        TYPE(Base(l1=LEN(c1%b_cmp%C1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 45
         IF ( Obj%l1 .NE. N ) STOP 46
         IF ( Obj%I1 .NE. 4 ) STOP 47
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 48
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 49
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 50
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 51
      END SUBROUTINE Sub16

      SUBROUTINE Sub21(N)
        INTEGER :: N
        TYPE(Base(l1=c2%b_cmp%l1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 52
         IF ( Obj%l1 .NE. N ) STOP 53
         IF ( Obj%I1 .NE. 4 ) STOP 54
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 55
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 56
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 57
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 58
      END SUBROUTINE Sub21

      SUBROUTINE Sub22(N)
        INTEGER :: N
        TYPE(Base(l1=c2%b_cmp%k1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 59
         IF ( Obj%l1 .NE. N ) STOP 60
         IF ( Obj%I1 .NE. 4 ) STOP 61
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 62
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 63
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 64
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 65
      END SUBROUTINE Sub22

      SUBROUTINE Sub23(N)
        INTEGER :: N
        TYPE(Base(l1=c2%b_cmp%I1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 66
         IF ( Obj%l1 .NE. N ) STOP 67
         IF ( Obj%I1 .NE. 4 ) STOP 68
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 69
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 70
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 71
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 72
      END SUBROUTINE Sub23

      SUBROUTINE Sub24(N)
        INTEGER :: N
        TYPE(Base(l1=c2%b_cmp%A1(1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 73
         IF ( Obj%l1 .NE. N ) STOP 74
         IF ( Obj%I1 .NE. 4 ) STOP 75
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 76
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 77
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 78
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 79
      END SUBROUTINE Sub24

      SUBROUTINE Sub25(N)
        INTEGER :: N
        TYPE(Base(l1=KIND(c2%b_cmp%I1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 80
         IF ( Obj%l1 .NE. N ) STOP 81
         IF ( Obj%I1 .NE. 4 ) STOP 82
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 83
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 84
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 85
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 86
      END SUBROUTINE Sub25

      SUBROUTINE Sub26(N)
        INTEGER :: N
        TYPE(Base(l1=LEN(c2%b_cmp%C1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 87
         IF ( Obj%l1 .NE. N ) STOP 88
         IF ( Obj%I1 .NE. 4 ) STOP 89
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 90
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 91
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 92
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 93
      END SUBROUTINE Sub26

END PROGRAM SpecExpHostAssociation02a
