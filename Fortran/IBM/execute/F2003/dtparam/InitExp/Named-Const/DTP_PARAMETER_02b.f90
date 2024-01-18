!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTP_PARAMETER_02b.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result 
!*  SECONDARY FUNCTIONS TESTED : Array constructor 
!*
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
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
        CLASS(Base(k1,l3)), POINTER :: cmp
      END TYPE
END MODULE
PROGRAM DTP_PARAMETER_02b
      USE Mod

      INTEGER, PARAMETER :: M = 10, L =5, P =2, K = 4 

      INTEGER, PARAMETER :: I10(M) = [(I, I = 1, M)], I5(L) = [(I**2, I = 1, L)]

      TYPE(Base(l1=M)), PARAMETER :: b1 = Base(l1=M) ( I10, 'b1constant' )
      TYPE(Child(l1=M,l2=L)), PARAMETER :: c1 = Child(l1=10,l2=L) ( 2*I10, 'c1const1', I5, 'c1const2' )
      TYPE(NextGen(l1=P,l2=L,l3=M)), PARAMETER :: n1 = NextGen(l1=P,l2=L,l3=M) &
                 ( [5, 6], 'AB', I5, 'CDEFG', I10, 'HIJKLMNOPQ', NULL() )

      TYPE(Base(l1=M)), TARGET :: tgt1 = b1 
      TYPE(Child(l1=M,l2=L)), TARGET :: tgt2 = c1 
      TYPE(NextGen(l1=P,l2=L,l3=M)), TARGET :: tgt3 = n1

      CLASS(Base(K,:)), POINTER :: poly 

      poly => tgt1
      IF ( .NOT. ASSOCIATED(poly, tgt1) ) STOP 10 
      IF ( SIZE(poly%A0)     .NE. M ) STOP 11
      IF ( LBOUND(poly%A0,1) .NE. 1 ) STOP 12
      IF ( UBOUND(poly%A0,1) .NE. M ) STOP 13
      IF ( LEN(poly%C0)      .NE. M ) STOP 14
      IF ( ANY(poly%A0   .NE.         I10) ) STOP 15
      IF ( TRIM(poly%C0) .NE. 'b1constant' ) STOP 16

      poly => tgt2
      IF ( .NOT. ASSOCIATED(poly, tgt2) ) STOP 20 
      SELECT TYPE ( poly )
          TYPE IS (Child(4,*,4,*))
            IF ( SIZE(poly%A0)     .NE. M ) STOP 21
            IF ( SIZE(poly%A1)     .NE. L ) STOP 22
            IF ( LBOUND(poly%A0,1) .NE. 1 ) STOP 23
            IF ( LBOUND(poly%A1,1) .NE. 1 ) STOP 24
            IF ( UBOUND(poly%A0,1) .NE. M ) STOP 25
            IF ( UBOUND(poly%A1,1) .NE. L ) STOP 26
            IF ( LEN(poly%C0)      .NE. M ) STOP 27
            IF ( LEN(poly%C1)      .NE. L ) STOP 28
            IF ( ANY(poly%A0   .NE.     2*I10) ) STOP 29
            IF ( ANY(poly%A1   .NE.        I5) ) STOP 30
            IF ( TRIM(poly%C0) .NE. 'c1const1' ) STOP 31
            IF ( TRIM(poly%C1) .NE.    'c1con' ) STOP 32

          CLASS DEFAULT
             STOP 34
      END SELECT

      poly => tgt3
      IF ( .NOT. ASSOCIATED(poly, tgt3) ) STOP 40 
      SELECT TYPE ( poly )
          TYPE IS (NextGen(4,*,4,*,*))
            IF ( SIZE(poly%A0)     .NE. P ) STOP 41
            IF ( SIZE(poly%A1)     .NE. L ) STOP 42
            IF ( SIZE(poly%A2)     .NE. M ) STOP 43
            IF ( LBOUND(poly%A0,1) .NE. 1 ) STOP 44
            IF ( LBOUND(poly%A1,1) .NE. 1 ) STOP 45
            IF ( LBOUND(poly%A2,1) .NE. 1 ) STOP 46
            IF ( UBOUND(poly%A0,1) .NE. P ) STOP 47
            IF ( UBOUND(poly%A1,1) .NE. L ) STOP 48
            IF ( UBOUND(poly%A2,1) .NE. M ) STOP 49
            IF ( LEN(poly%C0)      .NE. P ) STOP 50
            IF ( LEN(poly%C1)      .NE. L ) STOP 51
            IF ( LEN(poly%C2)      .NE. M ) STOP 52
            IF ( ANY(poly%A0   .NE.      [5, 6]) ) STOP 53
            IF ( ANY(poly%A1   .NE.          I5) ) STOP 54
            IF ( ANY(poly%A2   .NE.         I10) ) STOP 55
            IF ( TRIM(poly%C0) .NE.         'AB' ) STOP 56
            IF ( TRIM(poly%C1) .NE.      'CDEFG' ) STOP 57
            IF ( TRIM(poly%C2) .NE. 'HIJKLMNOPQ' ) STOP 58

            poly%cmp => tgt1
            IF ( .NOT. ASSOCIATED(poly%cmp, tgt1) ) STOP 59 
            ASSOCIATE ( a => poly%cmp )
                IF ( SIZE(a%A0)     .NE. M ) STOP 60
                IF ( LBOUND(a%A0,1) .NE. 1 ) STOP 61
                IF ( UBOUND(a%A0,1) .NE. M ) STOP 62
                IF ( LEN(a%C0)      .NE. M ) STOP 63
                IF ( ANY(a%A0   .NE.         I10) ) STOP 64
                IF ( TRIM(a%C0) .NE. 'b1constant' ) STOP 65
            END ASSOCIATE

            poly%cmp => tgt2
            IF ( .NOT. ASSOCIATED(poly%cmp, tgt2) ) STOP 66 
            SELECT TYPE ( s => poly%cmp )
                TYPE IS (Child(4,*,4,*))
                  IF ( SIZE(s%A0)     .NE. M ) STOP 67
                  IF ( SIZE(s%A1)     .NE. L ) STOP 68
                  IF ( LBOUND(s%A0,1) .NE. 1 ) STOP 69
                  IF ( LBOUND(s%A1,1) .NE. 1 ) STOP 70
                  IF ( UBOUND(s%A0,1) .NE. M ) STOP 71
                  IF ( UBOUND(s%A1,1) .NE. L ) STOP 72
                  IF ( LEN(s%C0)      .NE. M ) STOP 73
                  IF ( LEN(s%C1)      .NE. L ) STOP 74
                  IF ( ANY(s%A0   .NE.     2*I10) ) STOP 75
                  IF ( ANY(s%A1   .NE.        I5) ) STOP 76
                  IF ( TRIM(s%C0) .NE. 'c1const1' ) STOP 77
                  IF ( TRIM(s%C1) .NE.    'c1con' ) STOP 78

                CLASS DEFAULT
                  STOP 79
            END SELECT

          CLASS DEFAULT
             STOP 80
      END SELECT

END PROGRAM DTP_PARAMETER_02b
