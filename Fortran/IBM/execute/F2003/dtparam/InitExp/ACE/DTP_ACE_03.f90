!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTP_ACE_03.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : April 24, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array constructor with Type Specification 
!*  SECONDARY FUNCTIONS TESTED : 
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
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: A01(l1) = -1, A02(l1+1) = -2
        REAL(k1)    :: R0 = -0.1 
      END TYPE

      TYPE, EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 5

        INTEGER(k2) :: A2(l2) = -3
      END TYPE

      TYPE,  EXTENDS(Child) :: NextGen (l3)
        INTEGER, LEN  :: l3 = 2

        INTEGER(k2) :: A31(l1) = -4, A32(l1+l2) = -5 
        TYPE(Base(k1,l3)) :: bcomp
      END TYPE

END MODULE
PROGRAM DTP_ACE_03
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      CLASS(Base(4,:)), POINTER :: poly(:)
      LOGICAL, EXTERNAL :: precision_r4

      ALLOCATE( poly(2), SOURCE = [(Base(4,10)(), I=1,2)] )
      IF ( SIZE(poly) .NE. 2 ) STOP 10
      DO I = 1, 2
        IF ( SIZE(poly(I)%A01) .NE.  10 ) STOP 11
        IF ( SIZE(poly(I)%A02) .NE.  11 ) STOP 12
        IF ( LBOUND(poly(I)%A01,1) .NE.  1 ) STOP 13
        IF ( LBOUND(poly(I)%A02,1) .NE.  1 ) STOP 14
        IF ( UBOUND(poly(I)%A01,1) .NE. 10 ) STOP 15
        IF ( UBOUND(poly(I)%A02,1) .NE. 11 ) STOP 16
        IF ( ANY(poly(I)%A01   .NE. -1) ) STOP 17
        IF ( ANY(poly(I)%A02   .NE. -2) ) STOP 18
        IF ( .NOT. precision_r4(poly(I)%R0, -0.1) ) STOP 19
      END DO

      ALLOCATE( poly(10), SOURCE = [(Base(4,10)(2, 3, 0.2), I=1,10)] )
      IF ( SIZE(poly) .NE. 10 ) STOP 20
      DO I = 1, 10
        IF ( SIZE(poly(I)%A01) .NE.  10 ) STOP 21
        IF ( SIZE(poly(I)%A02) .NE.  11 ) STOP 22
        IF ( LBOUND(poly(I)%A01,1) .NE.  1 ) STOP 23
        IF ( LBOUND(poly(I)%A02,1) .NE.  1 ) STOP 24
        IF ( UBOUND(poly(I)%A01,1) .NE. 10 ) STOP 25
        IF ( UBOUND(poly(I)%A02,1) .NE. 11 ) STOP 26
        IF ( ANY(poly(I)%A01 .NE. 2) ) STOP 27
        IF ( ANY(poly(I)%A02 .NE. 3) ) STOP 28
        IF ( .NOT. precision_r4(poly(I)%R0, 0.2) ) STOP 29
      END DO

      ALLOCATE( poly(5), SOURCE = [(Child(4,3,4,3)(A01=10, A02=20, A2 = 30), I=1,5)] )
      IF ( SIZE(poly) .NE. 5 ) STOP 30
      SELECT TYPE ( poly )
          TYPE IS (Child(4,*,4,*))
            DO I = 1, 5
              IF ( SIZE(poly(I)%A01) .NE. 3 ) STOP 31
              IF ( SIZE(poly(I)%A02) .NE. 4 ) STOP 32
              IF ( SIZE(poly(I)%A2)  .NE. 3 ) STOP 33
              IF ( LBOUND(poly(I)%A01,1) .NE. 1 ) STOP 34
              IF ( LBOUND(poly(I)%A02,1) .NE. 1 ) STOP 35
              IF ( LBOUND(poly(I)%A2,1)  .NE. 1 ) STOP 36
              IF ( UBOUND(poly(I)%A01,1) .NE. 3 ) STOP 37
              IF ( UBOUND(poly(I)%A02,1) .NE. 4 ) STOP 38
              IF ( UBOUND(poly(I)%A2,1)  .NE. 3 ) STOP 39
              IF ( ANY(poly(I)%A01 .NE. 10) ) STOP 40
              IF ( ANY(poly(I)%A02 .NE. 20) ) STOP 41
              IF ( ANY(poly(I)%A2  .NE. 30) ) STOP 42
              IF ( .NOT. precision_r4(poly(I)%R0, -0.1) ) STOP 43
            END DO

          CLASS DEFAULT
             STOP 44
      END SELECT

      ALLOCATE( poly(1), SOURCE = [(NextGen(4,10,4,5,2)(11, 22, 0.12, 33, 44, 55, & 
                                & Base(4,2)(88, 99, 0.89) ), I=1,1)] )
      SELECT TYPE ( poly )
          TYPE IS (NextGen(4,*,4,*,*))
            IF ( SIZE(poly) .NE. 1 ) STOP 50
            IF ( SIZE(poly(1)%A01) .NE. 10 ) STOP 51
            IF ( SIZE(poly(1)%A02) .NE. 11 ) STOP 52
            IF ( SIZE(poly(1)%A2)  .NE.  5 ) STOP 53
            IF ( SIZE(poly(1)%A31) .NE. 10 ) STOP 54
            IF ( SIZE(poly(1)%A32) .NE. 15 ) STOP 55
            IF ( LBOUND(poly(1)%A01,1) .NE.  1 ) STOP 56
            IF ( LBOUND(poly(1)%A02,1) .NE.  1 ) STOP 57
            IF ( LBOUND(poly(1)%A2,1)  .NE.  1 ) STOP 58
            IF ( LBOUND(poly(1)%A31,1) .NE.  1 ) STOP 59
            IF ( LBOUND(poly(1)%A32,1) .NE.  1 ) STOP 60
            IF ( UBOUND(poly(1)%A01,1) .NE. 10 ) STOP 61
            IF ( UBOUND(poly(1)%A02,1) .NE. 11 ) STOP 62
            IF ( UBOUND(poly(1)%A2,1)  .NE.  5 ) STOP 63
            IF ( UBOUND(poly(1)%A31,1) .NE. 10 ) STOP 64
            IF ( UBOUND(poly(1)%A32,1) .NE. 15 ) STOP 65
            IF ( ANY(poly(1)%A01 .NE. 11) ) STOP 66
            IF ( ANY(poly(1)%A02 .NE. 22) ) STOP 67
            IF ( ANY(poly(1)%A2  .NE. 33) ) STOP 68
            IF ( ANY(poly(1)%A31 .NE. 44) ) STOP 69
            IF ( ANY(poly(1)%A32 .NE. 55) ) STOP 70
            IF ( .NOT. precision_r4(poly(1)%R0, 0.12) ) STOP 71

            IF ( poly(1)%bcomp%k1 .NE. 4 ) STOP 72
            IF ( poly(1)%bcomp%l1 .NE. 2 ) STOP 73
            IF ( SIZE(poly(1)%bcomp%A01) .NE. 2 ) STOP 74
            IF ( SIZE(poly(1)%bcomp%A02) .NE. 3 ) STOP 75
            IF ( LBOUND(poly(1)%bcomp%A01,1) .NE. 1 ) STOP 76
            IF ( LBOUND(poly(1)%bcomp%A02,1) .NE. 1 ) STOP 77
            IF ( UBOUND(poly(1)%bcomp%A01,1) .NE. 2 ) STOP 78
            IF ( UBOUND(poly(1)%bcomp%A02,1) .NE. 3 ) STOP 79
            IF ( ANY(poly(1)%bcomp%A01 .NE. 88) ) STOP 80
            IF ( ANY(poly(1)%bcomp%A02 .NE. 99) ) STOP 81
            IF ( .NOT. precision_r4(poly(1)%bcomp%R0, 0.89) ) STOP 82

          CLASS DEFAULT
             STOP 83
      END SELECT

END PROGRAM DTP_ACE_03
