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
!*  Defect 355394.3
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1)    :: A1(l1-1) = -1
        CHARACTER*(l1) :: C1 = 'C1'
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        INTEGER(k1)         :: A2(-1:k2-l1+2) = -2
        CHARACTER*(l1+2*l2) :: C2 = 'C2'
        TYPE(Base(k1,l1))   :: bcomp
      END TYPE

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3

        INTEGER(k1)    :: A3(k3/k2) = -3
        CHARACTER*(l3) :: C3= 'C3'
        CLASS(Base(k1,:)), ALLOCATABLE :: poly
      END TYPE
END MODULE

PROGRAM ExplicitInitExp06
      USE Mod
      IMPLICIT NONE

      TYPE(Base(4,5)) :: b2, b1 = Base(4,5) ( 10, 'AAAAA' )
      TYPE(Child(4,3,100,100)) :: c2, c1 = Child(4,3,100,100) ( 20,   &
                'BBBBB', 30, 'XL-compiler', Base(4,3) (40, 'CCCCC') )
      TYPE(NextGen(4,10,64,64,128,128)) :: n2, n1 =                   &
                NextGen(4,10,64,64,128,128) ( 11, 'Heisenberg',       &
                22, 'David Hilbert', Base(4,10) (33, 'Sommerfeld'),   &
                44, 'Ferdinand von Lindemann', NULL() )

      b2 = b1
      IF ( SIZE(b2%A1)     .NE.       4 ) ERROR STOP 10
      IF ( LBOUND(b2%A1,1) .NE.       1 ) ERROR STOP 11
      IF ( UBOUND(b2%A1,1) .NE.       4 ) ERROR STOP 12
      IF ( ANY(b2%A1       .NE.     10) ) ERROR STOP 13
      IF ( LEN(b2%C1)      .NE.       5 ) ERROR STOP 14
      IF ( TRIM(b2%C1)     .NE. 'AAAAA' ) ERROR STOP 15

      c2 = c1
      IF ( SIZE(c2%A1)     .NE.     2 ) ERROR STOP 20
      IF ( LBOUND(c2%A1,1) .NE.     1 ) ERROR STOP 21
      IF ( UBOUND(c2%A1,1) .NE.     2 ) ERROR STOP 22
      IF ( ANY(c2%A1       .NE.   20) ) ERROR STOP 23
      IF ( LEN(c2%C1)      .NE.     3 ) ERROR STOP 24
      IF ( TRIM(c2%C1)     .NE. 'BBB' ) ERROR STOP 25
      IF ( SIZE(c2%A2)     .NE.   101 ) ERROR STOP 26
      IF ( LBOUND(c2%A2,1) .NE.    -1 ) ERROR STOP 27
      IF ( UBOUND(c2%A2,1) .NE.    99 ) ERROR STOP 28
      IF ( ANY(c2%A2       .NE.   30) ) ERROR STOP 29
      IF ( LEN(c2%C2)      .NE.   203 ) ERROR STOP 30
      IF ( TRIM(c2%C2)   .NE. 'XL-compiler' ) ERROR STOP 31
      IF ( SIZE(c2%bcomp%A1)     .NE.     2 ) ERROR STOP 32
      IF ( LBOUND(c2%bcomp%A1,1) .NE.     1 ) ERROR STOP 33
      IF ( UBOUND(c2%bcomp%A1,1) .NE.     2 ) ERROR STOP 34
      IF ( ANY(c2%bcomp%A1       .NE.   40) ) ERROR STOP 35
      IF ( LEN(c2%bcomp%C1)      .NE.     3 ) ERROR STOP 36
      IF ( TRIM(c2%bcomp%C1)     .NE. 'CCC' ) ERROR STOP 37

      ALLOCATE( n1%poly, SOURCE = Base(4,10) (55, 'Rubinowicz') )
      n2 = n1
      IF ( SIZE(n2%A1)     .NE.            9 ) ERROR STOP 40
      IF ( LBOUND(n2%A1,1) .NE.            1 ) ERROR STOP 41
      IF ( UBOUND(n2%A1,1) .NE.            9 ) ERROR STOP 42
      IF ( ANY(n2%A1       .NE.          11) ) ERROR STOP 43
      IF ( LEN(n2%C1)      .NE.           10 ) ERROR STOP 44
      IF ( TRIM(n2%C1)     .NE. 'Heisenberg' ) ERROR STOP 45
      IF ( SIZE(n2%A2)     .NE.           58 ) ERROR STOP 46
      IF ( LBOUND(n2%A2,1) .NE.           -1 ) ERROR STOP 47
      IF ( UBOUND(n2%A2,1) .NE.           56 ) ERROR STOP 48
      IF ( ANY(n2%A2       .NE.          22) ) ERROR STOP 49
      IF ( LEN(n2%C2)      .NE.          138 ) ERROR STOP 50
      IF ( TRIM(n2%C2)  .NE. 'David Hilbert' ) ERROR STOP 51
      IF ( SIZE(n2%bcomp%A1)        .NE.     9 ) ERROR STOP 52
      IF ( LBOUND(n2%bcomp%A1,1)    .NE.     1 ) ERROR STOP 53
      IF ( UBOUND(n2%bcomp%A1,1)    .NE.     9 ) ERROR STOP 54
      IF ( ANY(n2%bcomp%A1          .NE.   33) ) ERROR STOP 55
      IF ( LEN(n2%bcomp%C1)         .NE.    10 ) ERROR STOP 56
      IF ( TRIM(n2%bcomp%C1) .NE. 'Sommerfeld' ) ERROR STOP 57
      IF ( SIZE(n2%A3)     .NE.            2 ) ERROR STOP 58
      IF ( LBOUND(n2%A3,1) .NE.            1 ) ERROR STOP 59
      IF ( UBOUND(n2%A3,1) .NE.            2 ) ERROR STOP 60
      IF ( ANY(n2%A3       .NE.          44) ) ERROR STOP 61
      IF ( LEN(n2%C3)      .NE.          128 ) ERROR STOP 62
      IF ( TRIM(n2%C3) .NE. 'Ferdinand von Lindemann' ) ERROR STOP 63
      IF ( SIZE(n2%poly%A1)        .NE.     9 ) ERROR STOP 64
      IF ( LBOUND(n2%poly%A1,1)    .NE.     1 ) ERROR STOP 65
      IF ( UBOUND(n2%poly%A1,1)    .NE.     9 ) ERROR STOP 66
      IF ( ANY(n2%poly%A1          .NE.   55) ) ERROR STOP 67
      IF ( LEN(n2%poly%C1)         .NE.    10 ) ERROR STOP 68
      IF ( TRIM(n2%poly%C1) .NE. 'Rubinowicz' ) ERROR STOP 69

END PROGRAM ExplicitInitExp06