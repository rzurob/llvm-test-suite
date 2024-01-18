!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : DTP_ACE_07.f
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10

        INTEGER(k1) :: A01(l1) = -1, A02(l1+1) = -2
      END TYPE

      TYPE :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 5

        INTEGER(k2) :: A2(2:l2) = -3
        TYPE(Base(k2,l2)) :: cmp(2:l2)
      END TYPE

      CONTAINS

      SUBROUTINE Sub(N)
        INTEGER :: N, I, J
        TYPE(Child(4,N)) :: Obj(N)

        Obj = [Child(4,N) :: (Child(4,N) (99, Base(4,N)(88, 77)), I=1,N)]
        IF ( Obj%k2    .NE. 4 ) STOP 10
        IF ( Obj%l2    .NE. N ) STOP 11
        IF ( SIZE(Obj) .NE. N ) STOP 12

        DO I = 1, N
             IF ( SIZE(Obj(I)%A2)      .NE. N-1 ) STOP 20
             IF ( SIZE(Obj(I)%cmp)     .NE. N-1 ) STOP 21
             IF ( LBOUND(Obj(I)%A2,1)  .NE.   2 ) STOP 22
             IF ( LBOUND(Obj(I)%cmp,1) .NE.   2 ) STOP 23
             IF ( UBOUND(Obj(I)%A2,1)  .NE.   N ) STOP 24
             IF ( UBOUND(Obj(I)%cmp,1) .NE.   N ) STOP 25
             IF ( ANY(Obj(I)%A2        .NE. 99) ) STOP 26
             DO J = LBOUND(Obj(I)%cmp,1), UBOUND(Obj(I)%cmp,1)
                  IF ( SIZE(Obj(I)%cmp(J)%A01)     .NE.   N ) STOP 30
                  IF ( SIZE(Obj(I)%cmp(J)%A02)     .NE. N+1 ) STOP 31
                  IF ( LBOUND(Obj(I)%cmp(J)%A01,1) .NE.   1 ) STOP 32
                  IF ( LBOUND(Obj(I)%cmp(J)%A02,1) .NE.   1 ) STOP 33
                  IF ( UBOUND(Obj(I)%cmp(J)%A01,1) .NE.   N ) STOP 34
                  IF ( UBOUND(Obj(I)%cmp(J)%A02,1) .NE. N+1 ) STOP 35
                  IF ( ANY(Obj(I)%cmp(J)%A01       .NE. 88) ) STOP 36
                  IF ( ANY(Obj(I)%cmp(J)%A02       .NE. 77) ) STOP 37
             END DO
        END DO

      END SUBROUTINE
END MODULE
PROGRAM DTP_ACE_07
      USE Mod
      IMPLICIT NONE

      CALL Sub(10)

      CALL Sub(4)

END PROGRAM DTP_ACE_07
