!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Init Expression - PARAMETER
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 9

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base-init'
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 10

        INTEGER(k2)   :: A1(l2) = -2
        CHARACTER(l2) :: C1 = 'Child-init'
      END TYPE

      TYPE,  EXTENDS(Child) :: NextGen (l3)
        INTEGER, LEN  :: l3 = 1024

        INTEGER(k1)   :: A2(l1) = -3
        CHARACTER(l2) :: C2 = 'NextGen-init'
      END TYPE

END MODULE
PROGRAM DTP_PARAMETER_03a
      USE Mod
      IMPLICIT TYPE (Base) (b)
      IMPLICIT TYPE (Child) (c)
      IMPLICIT TYPE (NextGen) (n)

      PARAMETER (b1 = Base ( 10, 'IBM' ))
      PARAMETER (c1 = Child ( C1 = 'Constant' ))
      PARAMETER (n1 = NextGen ( 3, 'ABC', 6, 'DEF', 9, 'GHI' ))

      IF ( b1%k1 .NE. 4 ) ERROR STOP 10
      IF ( b1%l1 .NE. 9 ) ERROR STOP 11
      IF ( SIZE(b1%A0) .NE.  9 ) ERROR STOP 12
      IF ( LEN(b1%C0)  .NE.  9 ) ERROR STOP 13
      IF ( ANY(b1%A0   .NE.   10) ) ERROR STOP 14
      IF ( TRIM(b1%C0) .NE. 'IBM' ) ERROR STOP 15

      IF ( b2%k1 .NE. 4 ) ERROR STOP 20
      IF ( b2%l1 .NE. 9 ) ERROR STOP 21
      IF ( SIZE(b2%A0) .NE.  9 ) ERROR STOP 22
      IF ( LEN(b2%C0)  .NE.  9 ) ERROR STOP 23
      IF ( ANY(b2%A0   .NE.        -1) ) ERROR STOP 24
      IF ( TRIM(b2%C0) .NE. 'Base-init') ERROR STOP 25

      IF ( c1%k1 .NE.  4 ) ERROR STOP 30
      IF ( c1%k2 .NE.  4 ) ERROR STOP 31
      IF ( c1%l1 .NE.  9 ) ERROR STOP 32
      IF ( c1%l2 .NE. 10 ) ERROR STOP 33
      IF ( SIZE(c1%A0) .NE.   9 ) ERROR STOP 34
      IF ( SIZE(c1%A1) .NE.  10 ) ERROR STOP 35
      IF ( LEN(c1%C0)  .NE.   9 ) ERROR STOP 36
      IF ( LEN(c1%C1)  .NE.  10 ) ERROR STOP 37
      IF ( ANY(c1%A0 .NE. -1) ) ERROR STOP 38
      IF ( ANY(c1%A1 .NE. -2) ) ERROR STOP 39
      IF ( TRIM(c1%C0) .NE. 'Base-init' ) ERROR STOP 40
      IF ( TRIM(c1%C1) .NE.  'Constant' ) ERROR STOP 41

      IF ( c2%k1 .NE.  4 ) ERROR STOP 50
      IF ( c2%k2 .NE.  4 ) ERROR STOP 51
      IF ( c2%l1 .NE.  9 ) ERROR STOP 52
      IF ( c2%l2 .NE. 10 ) ERROR STOP 53
      IF ( SIZE(c2%A0) .NE.   9 ) ERROR STOP 54
      IF ( SIZE(c2%A1) .NE.  10 ) ERROR STOP 55
      IF ( LEN(c2%C0)  .NE.   9 ) ERROR STOP 56
      IF ( LEN(c2%C1)  .NE.  10 ) ERROR STOP 57
      IF ( ANY(c2%A0 .NE. -1) ) ERROR STOP 58
      IF ( ANY(c2%A1 .NE. -2) ) ERROR STOP 59
      IF ( TRIM(c2%C0) .NE.  'Base-init' ) ERROR STOP 60
      IF ( TRIM(c2%C1) .NE. 'Child-init' ) ERROR STOP 61

      IF ( n1%k1 .NE.    4 ) ERROR STOP 70
      IF ( n1%k2 .NE.    4 ) ERROR STOP 71
      IF ( n1%l1 .NE.    9 ) ERROR STOP 72
      IF ( n1%l2 .NE.   10 ) ERROR STOP 73
      IF ( n1%l3 .NE. 1024 ) ERROR STOP 74
      IF ( SIZE(n1%A0) .NE.   9 ) ERROR STOP 75
      IF ( SIZE(n1%A1) .NE.  10 ) ERROR STOP 76
      IF ( SIZE(n1%A2) .NE.   9 ) ERROR STOP 77
      IF ( LEN(n1%C0)  .NE.   9 ) ERROR STOP 78
      IF ( LEN(n1%C1)  .NE.  10 ) ERROR STOP 79
      IF ( LEN(n1%C2)  .NE.  10 ) ERROR STOP 80
      IF ( ANY(n1%A0 .NE. 3) ) ERROR STOP 81
      IF ( ANY(n1%A1 .NE. 6) ) ERROR STOP 82
      IF ( ANY(n1%A2 .NE. 9) ) ERROR STOP 83
      IF ( TRIM(n1%C0) .NE. 'ABC' ) ERROR STOP 84
      IF ( TRIM(n1%C1) .NE. 'DEF' ) ERROR STOP 85
      IF ( TRIM(n1%C2) .NE. 'GHI' ) ERROR STOP 86

      IF ( n2%k1 .NE.    4 ) ERROR STOP 90
      IF ( n2%k2 .NE.    4 ) ERROR STOP 91
      IF ( n2%l1 .NE.    9 ) ERROR STOP 92
      IF ( n2%l2 .NE.   10 ) ERROR STOP 93
      IF ( n2%l3 .NE. 1024 ) ERROR STOP 94
      IF ( SIZE(n2%A0) .NE.   9 ) ERROR STOP 95
      IF ( SIZE(n2%A1) .NE.  10 ) ERROR STOP 96
      IF ( SIZE(n2%A2) .NE.   9 ) ERROR STOP 97
      IF ( LEN(n2%C0)  .NE.   9 ) ERROR STOP 98
      IF ( LEN(n2%C1)  .NE.  10 ) ERROR STOP 99
      IF ( LEN(n2%C2)  .NE.  10 ) ERROR STOP 100
      IF ( ANY(n2%A0 .NE. -1) ) ERROR STOP 101
      IF ( ANY(n2%A1 .NE. -2) ) ERROR STOP 102
      IF ( ANY(n2%A2 .NE. -3) ) ERROR STOP 103
      IF ( TRIM(n2%C0) .NE.  'Base-init' ) ERROR STOP 104
      IF ( TRIM(n2%C1) .NE. 'Child-init' ) ERROR STOP 105
      IF ( TRIM(n2%C2) .NE. 'NextGen-in' ) ERROR STOP 106

END PROGRAM DTP_PARAMETER_03a