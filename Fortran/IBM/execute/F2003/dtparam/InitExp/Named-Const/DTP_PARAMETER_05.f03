!*  ===================================================================
!*
!*  DATE                       : April 24, 2009
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
!* Defect 362080
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1

        INTEGER(k1)   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base-init'
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
        TYPE(Base(k1,l3)) :: bcomp
        TYPE(Child(k1,l3,k1,l3)) :: ccomp
      END TYPE

      TYPE Container (k,l)
        INTEGER, KIND :: k
        INTEGER, LEN  :: l

        CLASS(Base(k,l)), POINTER :: ptr
      END TYPE
END MODULE
PROGRAM DTP_PARAMETER_05
      USE Mod

      INTEGER, PARAMETER :: M = 10, L =5, K = 4, N = 2

      INTEGER, PARAMETER :: I10(M) = [(I, I = 1, M)], I5(L) = [(I**2, I = 1, L)]

      TYPE(Base(l1=M)),              PARAMETER :: b1 = Base(l1=M)              &
                 ( I10, 'b1constant' )
      TYPE(Child(l1=M,l2=L)),        PARAMETER :: c1 = Child(l1=10,l2=L)       &
                 ( 2*I10, 'c1const1', I5, 'c1const2' )
      TYPE(NextGen(l1=N,l2=L,l3=M)), PARAMETER :: n1 = NextGen(l1=N,l2=L,l3=M) &
                 ( 5, 'AB', 3, 'ABCDE', 1, 'n1constant', Base(l1=M)(), Child(l1=M,l2=M)() )

      TYPE(Base(l1=M)), TARGET :: btgt = b1
      TYPE(Child(l1=M,l2=L)), TARGET :: ctgt = c1
      TYPE(NextGen(l1=N,l2=L,l3=M)), TARGET :: ntgt = n1
      CLASS(Container(K,:)), POINTER :: poly

      IF ( SIZE(btgt%A0)     .NE. M ) ERROR STOP 10
      IF ( LBOUND(btgt%A0,1) .NE. 1 ) ERROR STOP 11
      IF ( UBOUND(btgt%A0,1) .NE. M ) ERROR STOP 12
      IF ( LEN(btgt%C0)      .NE. M ) ERROR STOP 13
      IF ( ANY(btgt%A0   .NE.         I10) ) ERROR STOP 14
      IF ( TRIM(btgt%C0) .NE. 'b1constant' ) ERROR STOP 15

      IF ( SIZE(ctgt%A0)     .NE. M ) ERROR STOP 16
      IF ( SIZE(ctgt%A1)     .NE. L ) ERROR STOP 17
      IF ( LBOUND(ctgt%A0,1) .NE. 1 ) ERROR STOP 18
      IF ( LBOUND(ctgt%A1,1) .NE. 1 ) ERROR STOP 19
      IF ( UBOUND(ctgt%A0,1) .NE. M ) ERROR STOP 20
      IF ( UBOUND(ctgt%A1,1) .NE. L ) ERROR STOP 21
      IF ( LEN(ctgt%C0)      .NE. M ) ERROR STOP 22
      IF ( LEN(ctgt%C1)      .NE. L ) ERROR STOP 23
      IF ( ANY(ctgt%A0   .NE.     2*I10) ) ERROR STOP 24
      IF ( ANY(ctgt%A1   .NE.        I5) ) ERROR STOP 25
      IF ( TRIM(ctgt%C0) .NE. 'c1const1' ) ERROR STOP 26
      IF ( TRIM(ctgt%C1) .NE.    'c1con' ) ERROR STOP 27

      IF ( SIZE(ntgt%A0)     .NE. 2 ) ERROR STOP 30
      IF ( SIZE(ntgt%A1)     .NE. L ) ERROR STOP 31
      IF ( SIZE(ntgt%A2)     .NE. M ) ERROR STOP 32
      IF ( LBOUND(ntgt%A0,1) .NE. 1 ) ERROR STOP 33
      IF ( LBOUND(ntgt%A1,1) .NE. 1 ) ERROR STOP 34
      IF ( LBOUND(ntgt%A2,1) .NE. 1 ) ERROR STOP 35
      IF ( UBOUND(ntgt%A0,1) .NE. 2 ) ERROR STOP 36
      IF ( UBOUND(ntgt%A1,1) .NE. L ) ERROR STOP 37
      IF ( UBOUND(ntgt%A2,1) .NE. M ) ERROR STOP 38
      IF ( LEN(ntgt%C0)      .NE. 2 ) ERROR STOP 39
      IF ( LEN(ntgt%C1)      .NE. L ) ERROR STOP 40
      IF ( LEN(ntgt%C2)      .NE. M ) ERROR STOP 41
      IF ( ANY(ntgt%A0   .NE. 5) ) ERROR STOP 42
      IF ( ANY(ntgt%A1   .NE. 3) ) ERROR STOP 43
      IF ( ANY(ntgt%A2   .NE. 1) ) ERROR STOP 44
      IF ( TRIM(ntgt%C0) .NE.         'AB' ) ERROR STOP 45
      IF ( TRIM(ntgt%C1) .NE.      'ABCDE' ) ERROR STOP 46
      IF ( TRIM(ntgt%C2) .NE. 'n1constant' ) ERROR STOP 47

      IF ( SIZE(ntgt%bcomp%A0)     .NE. M ) ERROR STOP 48
      IF ( LBOUND(ntgt%bcomp%A0,1) .NE. 1 ) ERROR STOP 49
      IF ( UBOUND(ntgt%bcomp%A0,1) .NE. M ) ERROR STOP 50
      IF ( LEN(ntgt%bcomp%C0)      .NE. M ) ERROR STOP 51
      IF ( ANY(ntgt%bcomp%A0   .NE.   -1) ) ERROR STOP 52
      IF ( TRIM(ntgt%bcomp%C0) .NE. 'Base-init' ) ERROR STOP 53

      IF ( SIZE(ntgt%ccomp%A0)     .NE. M ) ERROR STOP 54
      IF ( SIZE(ntgt%ccomp%A1)     .NE. M ) ERROR STOP 55
      IF ( LBOUND(ntgt%ccomp%A0,1) .NE. 1 ) ERROR STOP 56
      IF ( LBOUND(ntgt%ccomp%A1,1) .NE. 1 ) ERROR STOP 57
      IF ( UBOUND(ntgt%ccomp%A0,1) .NE. M ) ERROR STOP 58
      IF ( UBOUND(ntgt%ccomp%A1,1) .NE. M ) ERROR STOP 59
      IF ( LEN(ntgt%ccomp%C0)      .NE. M ) ERROR STOP 60
      IF ( LEN(ntgt%ccomp%C1)      .NE. M ) ERROR STOP 61
      IF ( ANY(ntgt%ccomp%A0   .NE.    -1) ) ERROR STOP 62
      IF ( ANY(ntgt%ccomp%A1   .NE.    -2) ) ERROR STOP 63
      IF ( TRIM(ntgt%ccomp%C0) .NE.        'Base-init' ) ERROR STOP 64
      IF ( TRIM(ntgt%ccomp%C1) .NE. 'Child-init' ) ERROR STOP 65

      ALLOCATE( Container(K,M) :: poly )
      poly%ptr => btgt
      IF ( SIZE(poly%ptr%A0)     .NE. M ) ERROR STOP 70
      IF ( LBOUND(poly%ptr%A0,1) .NE. 1 ) ERROR STOP 71
      IF ( UBOUND(poly%ptr%A0,1) .NE. M ) ERROR STOP 72
      IF ( LEN(poly%ptr%C0)      .NE. M ) ERROR STOP 73
      IF ( ANY(poly%ptr%A0   .NE.         I10) ) ERROR STOP 74
      IF ( TRIM(poly%ptr%C0) .NE. 'b1constant' ) ERROR STOP 75

      ALLOCATE( Container(K,M) :: poly )
      poly%ptr => ctgt
      SELECT TYPE ( s => poly%ptr )
        CLASS IS (Child(K,*,K,*))
          IF ( SIZE(s%A0)     .NE. M ) ERROR STOP 76
          IF ( SIZE(s%A1)     .NE. L ) ERROR STOP 77
          IF ( LBOUND(s%A0,1) .NE. 1 ) ERROR STOP 78
          IF ( LBOUND(s%A1,1) .NE. 1 ) ERROR STOP 79
          IF ( UBOUND(s%A0,1) .NE. M ) ERROR STOP 80
          IF ( UBOUND(s%A1,1) .NE. L ) ERROR STOP 81
          IF ( LEN(s%C0)      .NE. M ) ERROR STOP 82
          IF ( LEN(s%C1)      .NE. L ) ERROR STOP 83
          IF ( ANY(s%A0   .NE.     2*I10) ) ERROR STOP 84
          IF ( ANY(s%A1   .NE.        I5) ) ERROR STOP 85
          IF ( TRIM(s%C0) .NE. 'c1const1' ) ERROR STOP 86
          IF ( TRIM(s%C1) .NE.    'c1con' ) ERROR STOP 87

        CLASS DEFAULT
          STOP 88
      END SELECT

      ALLOCATE( Container(K,N) :: poly )
      poly%ptr => ntgt
      SELECT TYPE ( s => poly%ptr )
          CLASS IS (NextGen(K,*,K,*,*))
            IF ( SIZE(s%A0)     .NE. 2 ) ERROR STOP 90
            IF ( SIZE(s%A1)     .NE. L ) ERROR STOP 91
            IF ( SIZE(s%A2)     .NE. M ) ERROR STOP 92
            IF ( LBOUND(s%A0,1) .NE. 1 ) ERROR STOP 93
            IF ( LBOUND(s%A1,1) .NE. 1 ) ERROR STOP 94
            IF ( LBOUND(s%A2,1) .NE. 1 ) ERROR STOP 95
            IF ( UBOUND(s%A0,1) .NE. 2 ) ERROR STOP 96
            IF ( UBOUND(s%A1,1) .NE. L ) ERROR STOP 97
            IF ( UBOUND(s%A2,1) .NE. M ) ERROR STOP 98
            IF ( LEN(s%C0)      .NE. 2 ) ERROR STOP 99
            IF ( LEN(s%C1)      .NE. L ) ERROR STOP 100
            IF ( LEN(s%C2)      .NE. M ) ERROR STOP 101
            IF ( ANY(s%A0   .NE. 5) ) ERROR STOP 102
            IF ( ANY(s%A1   .NE. 3) ) ERROR STOP 103
            IF ( ANY(s%A2   .NE. 1) ) ERROR STOP 104
            IF ( TRIM(s%C0) .NE.         'AB' ) ERROR STOP 105
            IF ( TRIM(s%C1) .NE.      'ABCDE' ) ERROR STOP 106
            IF ( TRIM(s%C2) .NE. 'n1constant' ) ERROR STOP 107

            IF ( SIZE(s%bcomp%A0)     .NE. M ) ERROR STOP 108
            IF ( LBOUND(s%bcomp%A0,1) .NE. 1 ) ERROR STOP 109
            IF ( UBOUND(s%bcomp%A0,1) .NE. M ) ERROR STOP 110
            IF ( LEN(s%bcomp%C0)      .NE. M ) ERROR STOP 111
            IF ( ANY(s%bcomp%A0   .NE.   -1) ) ERROR STOP 112
            IF ( TRIM(s%bcomp%C0) .NE. 'Base-init' ) ERROR STOP 113

            IF ( SIZE(s%ccomp%A0)     .NE. M ) ERROR STOP 114
            IF ( SIZE(s%ccomp%A1)     .NE. M ) ERROR STOP 115
            IF ( LBOUND(s%ccomp%A0,1) .NE. 1 ) ERROR STOP 116
            IF ( LBOUND(s%ccomp%A1,1) .NE. 1 ) ERROR STOP 117
            IF ( UBOUND(s%ccomp%A0,1) .NE. M ) ERROR STOP 118
            IF ( UBOUND(s%ccomp%A1,1) .NE. M ) ERROR STOP 119
            IF ( LEN(s%ccomp%C0)      .NE. M ) ERROR STOP 120
            IF ( LEN(s%ccomp%C1)      .NE. M ) ERROR STOP 121
            IF ( ANY(s%ccomp%A0   .NE.    -1) ) ERROR STOP 122
            IF ( ANY(s%ccomp%A1   .NE.    -2) ) ERROR STOP 123
            IF ( TRIM(s%ccomp%C0) .NE. 'Base-init' ) ERROR STOP 124
            IF ( TRIM(s%ccomp%C1) .NE. 'Child-init' ) ERROR STOP 125

          CLASS DEFAULT
            STOP 126
      END SELECT

END PROGRAM DTP_PARAMETER_05