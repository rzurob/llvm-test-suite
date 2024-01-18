!*  ===================================================================
!*
!*  DATE                       : June 14, 2009
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
PROGRAM SpecExpHostAssociation01a
      IMPLICIT NONE

      TYPE Base (k1,l1)
         INTEGER, KIND :: k1 = 4
         INTEGER, LEN  :: l1 = 1

         INTEGER(k1) :: I1 = k1, A1(l1) = 2*k1
         CHARACTER(l1) :: C1 = 'B'
      END TYPE

      TYPE(Base) :: b1
      TYPE(Base(8,10)) :: b2

      CALL Sub11(1)
      CALL Sub12(4)
      CALL Sub13(4)
      CALL Sub14(8)
      CALL Sub15(4)
      CALL Sub16(1)

      CALL Sub21(10)
      CALL Sub22(8)
      CALL Sub23(8)
      CALL Sub24(16)
      CALL Sub25(8)
      CALL Sub26(10)

      b1 = Base (I1 = 10, A1 = 20)
      CALL Sub11(1)
      CALL Sub12(4)
      CALL Sub13(10)
      CALL Sub14(20)
      CALL Sub15(4)
      CALL Sub16(1)

      b2 = Base(8,10) (I1 = 11, A1 = 22)
      CALL Sub21(10)
      CALL Sub22(8)
      CALL Sub23(11)
      CALL Sub24(22)
      CALL Sub25(8)
      CALL Sub26(10)

      CONTAINS

      SUBROUTINE Sub11(N)
        INTEGER :: N
        TYPE(Base(l1=b1%l1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 10
         IF ( Obj%l1 .NE. N ) STOP 11
         IF ( Obj%I1 .NE. 4 ) STOP 12
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 13
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 14
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 15
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 16
      END SUBROUTINE Sub11

      SUBROUTINE Sub12(N)
        INTEGER :: N
        TYPE(Base(l1=b1%k1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 20
         IF ( Obj%l1 .NE. N ) STOP 21
         IF ( Obj%I1 .NE. 4 ) STOP 22
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 23
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 24
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 25
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 26
      END SUBROUTINE Sub12

      SUBROUTINE Sub13(N)
        INTEGER :: N
        TYPE(Base(l1=b1%I1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 27
         IF ( Obj%l1 .NE. N ) STOP 28
         IF ( Obj%I1 .NE. 4 ) STOP 29
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 30
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 31
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 32
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 33
      END SUBROUTINE Sub13

      SUBROUTINE Sub14(N)
        INTEGER :: N
        TYPE(Base(l1=b1%A1(1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 34
         IF ( Obj%l1 .NE. N ) STOP 35
         IF ( Obj%I1 .NE. 4 ) STOP 36
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 37
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 38
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 39
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 40
      END SUBROUTINE Sub14

      SUBROUTINE Sub15(N)
        INTEGER :: N
        TYPE(Base(l1=KIND(b1%I1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 41
         IF ( Obj%l1 .NE. N ) STOP 42
         IF ( Obj%I1 .NE. 4 ) STOP 43
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 44
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 45
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 46
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 47
      END SUBROUTINE Sub15

      SUBROUTINE Sub16(N)
        INTEGER :: N
        TYPE(Base(l1=LEN(b1%C1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 48
         IF ( Obj%l1 .NE. N ) STOP 49
         IF ( Obj%I1 .NE. 4 ) STOP 50
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 51
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 52
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 53
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 54
      END SUBROUTINE Sub16

      SUBROUTINE Sub21(N)
        INTEGER :: N
        TYPE(Base(l1=b2%l1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 55
         IF ( Obj%l1 .NE. N ) STOP 56
         IF ( Obj%I1 .NE. 4 ) STOP 57
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 58
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 59
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 60
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 61
      END SUBROUTINE Sub21

      SUBROUTINE Sub22(N)
        INTEGER :: N
        TYPE(Base(l1=b2%k1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 62
         IF ( Obj%l1 .NE. N ) STOP 63
         IF ( Obj%I1 .NE. 4 ) STOP 64
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 65
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 66
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 67
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 68
      END SUBROUTINE Sub22

      SUBROUTINE Sub23(N)
        INTEGER :: N
        TYPE(Base(l1=b2%I1)) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 69
         IF ( Obj%l1 .NE. N ) STOP 70
         IF ( Obj%I1 .NE. 4 ) STOP 71
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 72
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 73
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 74
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 75
      END SUBROUTINE Sub23

      SUBROUTINE Sub24(N)
        INTEGER :: N
        TYPE(Base(l1=b2%A1(1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 76
         IF ( Obj%l1 .NE. N ) STOP 77
         IF ( Obj%I1 .NE. 4 ) STOP 78
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 79
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 80
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 81
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 82
      END SUBROUTINE Sub24

      SUBROUTINE Sub25(N)
        INTEGER :: N
        TYPE(Base(l1=KIND(b2%I1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 83
         IF ( Obj%l1 .NE. N ) STOP 84
         IF ( Obj%I1 .NE. 4 ) STOP 85
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 86
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 87
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 88
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 89
      END SUBROUTINE Sub25

      SUBROUTINE Sub26(N)
        INTEGER :: N
        TYPE(Base(l1=LEN(b2%C1))) :: Obj

         IF ( Obj%k1 .NE. 4 ) STOP 90
         IF ( Obj%l1 .NE. N ) STOP 91
         IF ( Obj%I1 .NE. 4 ) STOP 92
         IF ( SIZE(Obj%A1) .NE.   N ) STOP 93
         IF ( ANY(Obj%A1   .NE.  8) ) STOP 94
         IF ( LEN(Obj%C1)  .NE.   N ) STOP 95
         IF ( TRIM(Obj%C1) .NE. 'B' ) STOP 96
      END SUBROUTINE Sub26

END PROGRAM SpecExpHostAssociation01a
