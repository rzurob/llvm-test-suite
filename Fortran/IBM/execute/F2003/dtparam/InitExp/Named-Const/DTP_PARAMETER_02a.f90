!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result
!*  SECONDARY FUNCTIONS TESTED : Array constructor
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
        INTEGER, LEN  :: l1

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'XLF'
      END TYPE


      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2

        INTEGER(k1)   :: A1(l2) = -2
        CHARACTER(l2) :: C1 = 'Child-init'
      END TYPE

      TYPE,  EXTENDS(Child) :: NextGen (l3)
        INTEGER, LEN  :: l3

        INTEGER(k1)   :: A2(l3) = -3
        CHARACTER(l3) :: C2 = 'NextGen-init'
      END TYPE
END MODULE
PROGRAM DTP_PARAMETER_02a
      USE Mod

      INTEGER, PARAMETER :: M = 10, L =5, P =2, K = 4

      INTEGER, PARAMETER :: I10(M) = [(I, I = 1, M)], I5(L) = [(I**2, I = 1, L)]

      TYPE(Base(l1=M)), PARAMETER :: b1 = Base(l1=M) ( I10, 'b1constant' )
      TYPE(Child(l1=M,l2=L)), PARAMETER :: c1 = Child(l1=10,l2=L) ( 2*I10, 'c1const1', I5, 'c1const2' )
      TYPE(NextGen(l1=P,l2=L,l3=M)), PARAMETER :: n1 = NextGen(l1=P,l2=L,l3=M) &
                 ( [5, 6], 'AB', I5, 'CDEFG', I10, 'HIJKLMNOPQ' )

      CLASS(Base(K,:)), POINTER :: poly

      IF ( SIZE(b1%A0)     .NE. M ) STOP 10
      IF ( LBOUND(b1%A0,1) .NE. 1 ) STOP 11
      IF ( UBOUND(b1%A0,1) .NE. M ) STOP 12
      IF ( LEN(b1%C0)      .NE. M ) STOP 13
      IF ( ANY(b1%A0   .NE.         I10) ) STOP 14
      IF ( TRIM(b1%C0) .NE. 'b1constant' ) STOP 15

      IF ( SIZE(c1%A0)     .NE. M ) STOP 20
      IF ( SIZE(c1%A1)     .NE. L ) STOP 21
      IF ( LBOUND(c1%A0,1) .NE. 1 ) STOP 22
      IF ( LBOUND(c1%A1,1) .NE. 1 ) STOP 23
      IF ( UBOUND(c1%A0,1) .NE. M ) STOP 24
      IF ( UBOUND(c1%A1,1) .NE. L ) STOP 25
      IF ( LEN(c1%C0)      .NE. M ) STOP 26
      IF ( LEN(c1%C1)      .NE. L ) STOP 27
      IF ( ANY(c1%A0   .NE.     2*I10) ) STOP 28
      IF ( ANY(c1%A1   .NE.        I5) ) STOP 29
      IF ( TRIM(c1%C0) .NE. 'c1const1' ) STOP 30
      IF ( TRIM(c1%C1) .NE.    'c1con' ) STOP 31

      IF ( SIZE(n1%A0)     .NE. P ) STOP 40
      IF ( SIZE(n1%A1)     .NE. L ) STOP 41
      IF ( SIZE(n1%A2)     .NE. M ) STOP 42
      IF ( LBOUND(n1%A0,1) .NE. 1 ) STOP 43
      IF ( LBOUND(n1%A1,1) .NE. 1 ) STOP 44
      IF ( LBOUND(n1%A2,1) .NE. 1 ) STOP 45
      IF ( UBOUND(n1%A0,1) .NE. P ) STOP 46
      IF ( UBOUND(n1%A1,1) .NE. L ) STOP 47
      IF ( UBOUND(n1%A2,1) .NE. M ) STOP 48
      IF ( LEN(n1%C0)      .NE. P ) STOP 49
      IF ( LEN(n1%C1)      .NE. L ) STOP 50
      IF ( LEN(n1%C2)      .NE. M ) STOP 51
      IF ( ANY(n1%A0   .NE.      [5, 6]) ) STOP 52
      IF ( ANY(n1%A1   .NE.          I5) ) STOP 53
      IF ( ANY(n1%A2   .NE.         I10) ) STOP 54
      IF ( TRIM(n1%C0) .NE.         'AB' ) STOP 55
      IF ( TRIM(n1%C1) .NE.      'CDEFG' ) STOP 56
      IF ( TRIM(n1%C2) .NE. 'HIJKLMNOPQ' ) STOP 57

      ALLOCATE( poly, SOURCE = b1 )
      IF ( SIZE(poly%A0)     .NE. M ) STOP 60
      IF ( LBOUND(poly%A0,1) .NE. 1 ) STOP 61
      IF ( UBOUND(poly%A0,1) .NE. M ) STOP 62
      IF ( LEN(poly%C0)      .NE. M ) STOP 63
      IF ( ANY(poly%A0   .NE.         I10) ) STOP 64
      IF ( TRIM(poly%C0) .NE. 'b1constant' ) STOP 65

      ALLOCATE( poly, SOURCE = c1 )
      SELECT TYPE ( poly )
          TYPE IS (Child(4,*,4,*))
            IF ( SIZE(poly%A0)     .NE. M ) STOP 70
            IF ( SIZE(poly%A1)     .NE. L ) STOP 71
            IF ( LBOUND(poly%A0,1) .NE. 1 ) STOP 72
            IF ( LBOUND(poly%A1,1) .NE. 1 ) STOP 73
            IF ( UBOUND(poly%A0,1) .NE. M ) STOP 74
            IF ( UBOUND(poly%A1,1) .NE. L ) STOP 75
            IF ( LEN(poly%C0)      .NE. M ) STOP 76
            IF ( LEN(poly%C1)      .NE. L ) STOP 77
            IF ( ANY(poly%A0   .NE.     2*I10) ) STOP 78
            IF ( ANY(poly%A1   .NE.        I5) ) STOP 79
            IF ( TRIM(poly%C0) .NE. 'c1const1' ) STOP 80
            IF ( TRIM(poly%C1) .NE.    'c1con' ) STOP 81

          CLASS DEFAULT
             STOP 82
      END SELECT

      ALLOCATE( poly, SOURCE = n1 )
      SELECT TYPE ( poly )
          TYPE IS (NextGen(4,*,4,*,*))
            IF ( SIZE(poly%A0)     .NE. P ) STOP 90
            IF ( SIZE(poly%A1)     .NE. L ) STOP 91
            IF ( SIZE(poly%A2)     .NE. M ) STOP 92
            IF ( LBOUND(poly%A0,1) .NE. 1 ) STOP 93
            IF ( LBOUND(poly%A1,1) .NE. 1 ) STOP 94
            IF ( LBOUND(poly%A2,1) .NE. 1 ) STOP 95
            IF ( UBOUND(poly%A0,1) .NE. P ) STOP 96
            IF ( UBOUND(poly%A1,1) .NE. L ) STOP 97
            IF ( UBOUND(poly%A2,1) .NE. M ) STOP 98
            IF ( LEN(poly%C0)      .NE. P ) STOP 99
            IF ( LEN(poly%C1)      .NE. L ) STOP 100
            IF ( LEN(poly%C2)      .NE. M ) STOP 101
            IF ( ANY(poly%A0   .NE.      [5, 6]) ) STOP 102
            IF ( ANY(poly%A1   .NE.          I5) ) STOP 103
            IF ( ANY(poly%A2   .NE.         I10) ) STOP 104
            IF ( TRIM(poly%C0) .NE.         'AB' ) STOP 105
            IF ( TRIM(poly%C1) .NE.      'CDEFG' ) STOP 106
            IF ( TRIM(poly%C2) .NE. 'HIJKLMNOPQ' ) STOP 107

          CLASS DEFAULT
             STOP 108
      END SELECT

END PROGRAM DTP_PARAMETER_02a
