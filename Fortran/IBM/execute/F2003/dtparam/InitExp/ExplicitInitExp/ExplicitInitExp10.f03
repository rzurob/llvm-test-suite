!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Explicit Init Expression
!*  SECONDARY FUNCTIONS TESTED : Defined assignment
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Defect 362080
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1)    :: A1(l1+1) = -1
        CHARACTER*(l1) :: C1 = 'C1'

        CONTAINS
        PROCEDURE, PASS :: assgnA => assgnBase
        GENERIC :: assignment(=) => assgnA
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k1)         :: A2(l2+2) = -2
        CHARACTER*(l1+l2) :: C2 = 'C2'
        TYPE(Base(k1,l1))   :: bcomp

        CONTAINS
        PROCEDURE, PASS :: assgnB => assgnChild
        GENERIC :: assignment(=) => assgnB
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3

        INTEGER(k1)    :: A3(l1) = -3
        CHARACTER*(l1) :: C3= 'C3'
        CLASS(Base(k1,:)), ALLOCATABLE :: poly

        CONTAINS
        PROCEDURE, PASS :: assgnC => assgnNextGen
        GENERIC :: assignment(=) => assgnC
      END TYPE

      CONTAINS

     SUBROUTINE assgnBase(this, arg)
       CLASS(Base(4,*)), INTENT(OUT) :: this
       TYPE(Base(4,*)), INTENT(IN) :: arg

       this%A1 = arg%A1
       this%C1 = arg%C1
     END SUBROUTINE

     SUBROUTINE assgnChild(this, arg)
       CLASS(Child(4,*,64,*)), INTENT(OUT) :: this
       TYPE(Child(4,*,64,*)), INTENT(IN) :: arg

       this%A1 = arg%A1
       this%C1 = arg%C1
       this%A2 = arg%A2
       this%C2 = arg%C2
       this%bcomp%A1 = arg%bcomp%A1
       this%bcomp%C1 = arg%bcomp%C1
     END SUBROUTINE

     SUBROUTINE assgnNextGen(this, arg)
       CLASS(NextGen(4,*,64,*,128,*)), INTENT(OUT) :: this
       TYPE(NextGen(4,*,64,*,128,*)), INTENT(IN) :: arg

       this%A1 = arg%A1
       this%C1 = arg%C1
       this%A2 = arg%A2
       this%C2 = arg%C2
       this%bcomp%A1 = arg%bcomp%A1
       this%bcomp%C1 = arg%bcomp%C1
       this%A3 = arg%A3
       this%C3 = arg%C3
       IF ( ALLOCATED( arg%poly ) ) ALLOCATE( this%poly, source = arg%poly )

     END SUBROUTINE

END MODULE
PROGRAM ExplicitInitExp10
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,5)) :: b1 = Base(4,5) ( 10, 'AAAAA' ), b2
      TYPE(Child(4,5,64,64)) :: c1 = Child(4,5,64,64) ( 20, 'BBBBB',  &
                30, 'XL-compiler', Base(4,5) (40, 'CCCCC') ), c2
      TYPE(NextGen(4,5,64,64,128,128)) :: n2, n1 =                   &
                NextGen(4,5,64,64,128,128) ( 11, 'Heisenberg',       &
                22, 'David Hilbert', Base(4,5) (33, 'Sommerfeld'),       &
                44, 'Ferdinand', NULL() )

      b2 = b1
      IF ( SIZE(b2%A1)     .NE.       6 ) ERROR STOP 10
      IF ( ANY(b2%A1       .NE.     10) ) ERROR STOP 11
      IF ( LEN(b2%C1)      .NE.       5 ) ERROR STOP 12
      IF ( TRIM(b2%C1)     .NE. 'AAAAA' ) ERROR STOP 13

      c2 = c1
      IF ( SIZE(c2%A1)     .NE.       6 ) ERROR STOP 14
      IF ( ANY(c2%A1       .NE.     20) ) ERROR STOP 15
      IF ( LEN(c2%C1)      .NE.       5 ) ERROR STOP 16
      IF ( TRIM(c2%C1)     .NE. 'BBBBB' ) ERROR STOP 17
      IF ( SIZE(c2%A2)     .NE.      66 ) ERROR STOP 18
      IF ( ANY(c2%A2       .NE.     30) ) ERROR STOP 19
      IF ( LEN(c2%C2)      .NE.      69 ) ERROR STOP 20
      IF ( TRIM(c2%C2)   .NE. 'XL-compiler' ) ERROR STOP 21
      IF ( SIZE(c2%bcomp%A1)     .NE.     6 ) ERROR STOP 22
      IF ( ANY(c2%bcomp%A1       .NE.   40) ) ERROR STOP 23
      IF ( LEN(c2%bcomp%C1)      .NE.     5 ) ERROR STOP 24
      IF ( TRIM(c2%bcomp%C1)  .NE. 'CCCCC' ) ERROR STOP 25

      c2 = b1
      IF ( SIZE(c2%A1)     .NE.       6 ) ERROR STOP 26
      IF ( ANY(c2%A1       .NE.     10) ) ERROR STOP 27
      IF ( LEN(c2%C1)      .NE.       5 ) ERROR STOP 28
      IF ( TRIM(c2%C1)     .NE. 'AAAAA' ) ERROR STOP 29
      IF ( SIZE(c2%A2)     .NE.      66 ) ERROR STOP 30
      IF ( ANY(c2%A2       .NE.     -2) ) ERROR STOP 31
      IF ( LEN(c2%C2)      .NE.      69 ) ERROR STOP 32
      IF ( TRIM(c2%C2)     .NE.    'C2' ) ERROR STOP 33
      IF ( SIZE(c2%bcomp%A1)     .NE.     6 ) ERROR STOP 34
      IF ( ANY(c2%bcomp%A1       .NE.   -1) ) ERROR STOP 35
      IF ( LEN(c2%bcomp%C1)      .NE.     5 ) ERROR STOP 36
      IF ( TRIM(c2%bcomp%C1)     .NE.  'C1' ) ERROR STOP 37

      allocate (base(4,5) :: n1%poly)
      n1%poly = Base(4,5) (55, 'Rubinowicz')
      !ALLOCATE( n1%poly, SOURCE = Base(4,5) (55, 'Rubinowicz') )
      n2 = n1
      IF ( SIZE(n2%A1)     .NE.       6 ) ERROR STOP 38
      IF ( ANY(n2%A1       .NE.     11) ) ERROR STOP 39
      IF ( LEN(n2%C1)      .NE.       5 ) ERROR STOP 40
      IF ( TRIM(n2%C1)     .NE. 'Heise' ) ERROR STOP 41
      IF ( SIZE(n2%A2)     .NE.      66 ) ERROR STOP 42
      IF ( ANY(n2%A2       .NE.     22) ) ERROR STOP 43
      IF ( LEN(n2%C2)      .NE.      69 ) ERROR STOP 44
      IF ( TRIM(n2%C2)    .NE. 'David Hilbert' ) ERROR STOP 45
      IF ( SIZE(n2%bcomp%A1) .NE.       6 ) ERROR STOP 46
      IF ( ANY(n2%bcomp%A1   .NE.     33) ) ERROR STOP 47
      IF ( LEN(n2%bcomp%C1)  .NE.       5 ) ERROR STOP 48
      IF ( TRIM(n2%bcomp%C1) .NE. 'Somme' ) ERROR STOP 49
      IF ( SIZE(n2%A3)       .NE.       5 ) ERROR STOP 50
      IF ( ANY(n2%A3         .NE.     44) ) ERROR STOP 51
      IF ( LEN(n2%C3)        .NE.       5 ) ERROR STOP 52
      IF ( TRIM(n2%C3)       .NE. 'Ferdi' ) ERROR STOP 53
      IF ( SIZE(n2%poly%A1)    .NE.     6 ) ERROR STOP 54
      IF ( ANY(n2%poly%A1      .NE.   55) ) ERROR STOP 55
      IF ( LEN(n2%poly%C1)     .NE.     5 ) ERROR STOP 56
      IF ( TRIM(n2%poly%C1)  .NE. 'Rubin' ) ERROR STOP 57

      n2 = b1
      IF ( SIZE(n2%A1)     .NE.       6 ) ERROR STOP 58
      IF ( ANY(n2%A1       .NE.     10) ) ERROR STOP 59
      IF ( LEN(n2%C1)      .NE.       5 ) ERROR STOP 60
      IF ( TRIM(n2%C1)     .NE. 'AAAAA' ) ERROR STOP 61
      IF ( SIZE(n2%A2)     .NE.      66 ) ERROR STOP 62
      IF ( ANY(n2%A2       .NE.     -2) ) ERROR STOP 63
      IF ( LEN(n2%C2)      .NE.      69 ) ERROR STOP 64
      IF ( TRIM(n2%C2)     .NE.    'C2' ) ERROR STOP 65
      IF ( SIZE(n2%bcomp%A1) .NE.     6 ) ERROR STOP 66
      IF ( ANY(n2%bcomp%A1   .NE.   -1) ) ERROR STOP 67
      IF ( LEN(n2%bcomp%C1)  .NE.     5 ) ERROR STOP 68
      IF ( TRIM(n2%bcomp%C1) .NE.  'C1' ) ERROR STOP 69
      IF ( SIZE(n2%A3)       .NE.     5 ) ERROR STOP 70
      IF ( ANY(n2%A3         .NE.   -3) ) ERROR STOP 71
      IF ( LEN(n2%C3)        .NE.     5 ) ERROR STOP 72
      IF ( TRIM(n2%C3)       .NE.  'C3' ) ERROR STOP 73
      IF ( ALLOCATED(n2%poly) ) ERROR STOP 74

      n2 = c1
      IF ( SIZE(n2%A1)     .NE.       6 ) ERROR STOP 75
      IF ( ANY(n2%A1       .NE.     20) ) ERROR STOP 76
      IF ( LEN(n2%C1)      .NE.       5 ) ERROR STOP 77
      IF ( TRIM(n2%C1)     .NE. 'BBBBB' ) ERROR STOP 78
      IF ( SIZE(n2%A2)     .NE.      66 ) ERROR STOP 79
      IF ( ANY(n2%A2       .NE.     30) ) ERROR STOP 80
      IF ( LEN(n2%C2)      .NE.      69 ) ERROR STOP 81
      IF ( TRIM(n2%C2)   .NE. 'XL-compiler' ) ERROR STOP 82
      IF ( SIZE(n2%bcomp%A1)     .NE.     6 ) ERROR STOP 83
      IF ( ANY(n2%bcomp%A1       .NE.   40) ) ERROR STOP 84
      IF ( LEN(n2%bcomp%C1)      .NE.     5 ) ERROR STOP 85
      IF ( TRIM(n2%bcomp%C1)  .NE. 'CCCCC' ) ERROR STOP 86
      IF ( SIZE(n2%A3)       .NE.    5 ) ERROR STOP 87
      IF ( ANY(n2%A3         .NE.  -3) ) ERROR STOP 88
      IF ( LEN(n2%C3)        .NE.    5 ) ERROR STOP 89
      IF ( TRIM(n2%C3)       .NE. 'C3' ) ERROR STOP 90
      IF ( ALLOCATED(n2%poly) ) ERROR STOP 91

      IF ( ALLOCATED( n1%poly ) ) DEALLOCATE (n1%poly)
      ALLOCATE( n1%poly, SOURCE = c1 )
      n2 = n1
      IF ( SIZE(n2%A1)     .NE.       6 ) ERROR STOP 92
      IF ( ANY(n2%A1       .NE.     11) ) ERROR STOP 93
      IF ( LEN(n2%C1)      .NE.       5 ) ERROR STOP 94
      IF ( TRIM(n2%C1)     .NE. 'Heise' ) ERROR STOP 95
      IF ( SIZE(n2%A2)     .NE.      66 ) ERROR STOP 96
      IF ( ANY(n2%A2       .NE.     22) ) ERROR STOP 97
      IF ( LEN(n2%C2)      .NE.      69 ) ERROR STOP 98
      IF ( TRIM(n2%C2) .NE. 'David Hilbert' ) ERROR STOP 99
      IF ( SIZE(n2%bcomp%A1)  .NE.        6 ) ERROR STOP 100
      IF ( ANY(n2%bcomp%A1    .NE.      33) ) ERROR STOP 101
      IF ( LEN(n2%bcomp%C1)   .NE.        5 ) ERROR STOP 102
      IF ( TRIM(n2%bcomp%C1) .NE. 'Somme' ) ERROR STOP 103
      IF ( SIZE(n2%A3)        .NE.        5 ) ERROR STOP 104
      IF ( ANY(n2%A3          .NE.      44) ) ERROR STOP 105
      IF ( LEN(n2%C3)         .NE.        5 ) ERROR STOP 106
      IF ( TRIM(n2%C3)        .NE.  'Ferdi' ) ERROR STOP 107

      SELECT TYPE ( s => n2%poly )
        CLASS IS (Child(4,*,64,*))
          IF ( SIZE(s%A1)    .NE.     6 ) ERROR STOP 108
          IF ( ANY(s%A1      .NE.   20) ) ERROR STOP 109
          IF ( LEN(s%C1)     .NE.     5 ) ERROR STOP 110
          IF ( TRIM(s%C1)  .NE. 'BBBBB' ) ERROR STOP 111
          IF ( SIZE(s%A2)    .NE.    66 ) ERROR STOP 108
          IF ( ANY(s%A2      .NE.   30) ) ERROR STOP 109
          IF ( LEN(s%C2)     .NE.    69 ) ERROR STOP 110
          IF ( TRIM(s%C2)  .NE. 'XL-compiler' ) ERROR STOP 111
          IF ( SIZE(s%bcomp%A1)    .NE.     6 ) ERROR STOP 112
          IF ( ANY(s%bcomp%A1      .NE.   40) ) ERROR STOP 113
          IF ( LEN(s%bcomp%C1)     .NE.     5 ) ERROR STOP 114
          IF ( TRIM(s%bcomp%C1)  .NE. 'CCCCC' ) ERROR STOP 115

        CLASS DEFAULT
           STOP 116
      END SELECT

END PROGRAM ExplicitInitExp10