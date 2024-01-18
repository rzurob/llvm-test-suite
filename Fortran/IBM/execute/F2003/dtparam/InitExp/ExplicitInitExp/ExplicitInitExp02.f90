!*  ===================================================================
!*
!*  DATE                       : April 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Explicit Init. Exp.
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
MODULE Mod1
      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(LEN=l1) :: Carr(l1)
        INTEGER(KIND=k1)  :: A0(l1), A1(2*l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,l1)), POINTER :: Cmp
      END TYPE Child

      INTEGER, PARAMETER :: knd = 4, len = 10
      TYPE(Base(knd,len)), TARGET :: Tgt = (Base(knd,len) ([('A'//CHAR(I+64), I = 1, len)], &
                                           [(I, I = 1, len)], [(I**2, I = 1, 2*len)]))
      TYPE(Base(knd,len)), TARGET :: cbl = (Base(knd,len) ([(CHAR(I+64), I = 1, len)],          &
                                           [(I-1, I = 1, len)], [(I+2, I = 1, 2*len)]))

      CONTAINS

      SUBROUTINE sub1(Arg)
        CLASS(*), POINTER, INTENT(INOUT) :: Arg

        ALLOCATE( Arg, SOURCE = (Child(knd,len) ( Carr = 'C', A0 = 1, A1 = 2, Cmp = Tgt )) )
        IF ( .NOT. ASSOCIATED(Arg) ) STOP 10

        SELECT TYPE ( A => Arg )
          CLASS IS (Child(knd,*))
             IF ( A%k1 .NE. knd ) STOP 11
             IF ( A%l1 .NE. len ) STOP 12
             IF ( SIZE(A%Carr) .NE. len ) STOP 13
             IF ( SIZE(A%A0) .NE.   len ) STOP 14
             IF ( SIZE(A%A1) .NE. 2*len ) STOP 15
             IF ( ANY(A%Carr .NE. 'C') ) STOP 16
             IF ( ANY(A%A0   .NE.   1) ) STOP 17
             IF ( ANY(A%A1   .NE.   2) ) STOP 18

             print*, A%Cmp%Carr
             print*, A%Cmp%A0
             print*, A%Cmp%A1
             IF ( .NOT. ASSOCIATED(A%Cmp, Tgt ) ) STOP 20
             IF ( A%Cmp%k1 .NE. knd ) STOP 21
             IF ( A%Cmp%l1 .NE. len ) STOP 22
             IF ( SIZE(A%Cmp%Carr) .NE. len ) STOP 23
             IF ( SIZE(A%Cmp%A0) .NE.   len ) STOP 24
             IF ( SIZE(A%Cmp%A1) .NE. 2*len ) STOP 25
             IF ( ANY(A%Cmp%Carr .NE. [('A'//CHAR(I+64), I = 1, len)]) ) STOP 26  ! test with ANY is passing
             IF ( ANY(A%Cmp%A0   .NE.               [(I, I = 1, len)]) ) STOP 27  ! because the loop is not
             IF ( ANY(A%Cmp%A1   .NE.          [(I**2, I = 1, 2*len)]) ) STOP 28  ! executed

             A%Carr = [CHARACTER(len) :: 'I am not c', 'ertain wha', 't the lang', &
                                         'uage for s', 'cientific ' ,'computatio', &
                                         'n will loo', 'k like by ', 'the 21st c', 'entury... ']
             A%A0 = [(I, I = 1, len)]
             A%A1 = [(I, I = 1, 2*len)]
             A%Cmp => cbl

          CLASS DEFAULT
             STOP 29
        END SELECT

      END SUBROUTINE sub1

END MODULE Mod1
PROGRAM ExplocitInitExp02
      USE Mod1
      IMPLICIT NONE

      CLASS(*), POINTER :: dtv
      integer i

      CALL sub1(dtv)

      SELECT TYPE ( dtv )
        CLASS IS (Child(knd,*))
           IF ( dtv%k1 .NE. knd ) STOP 30
           IF ( dtv%l1 .NE. len ) STOP 31
           IF ( SIZE(dtv%Carr) .NE. len ) STOP 32
           IF ( SIZE(dtv%A0) .NE.   len ) STOP 33
           IF ( SIZE(dtv%A1) .NE. 2*len ) STOP 34
           IF ( ANY(dtv%A0   .NE.   [(I, I = 1, len)]) ) STOP 35
           IF ( ANY(dtv%A1   .NE. [(I, I = 1, 2*len)]) ) STOP 36
           print*, dtv%Carr
           print*, dtv%A0
           print*, dtv%A1

           print*, dtv%Cmp%Carr
           print*, dtv%Cmp%A0
           print*, dtv%Cmp%A1
           IF ( .NOT. ASSOCIATED(dtv%Cmp, cbl ) ) STOP 37
           IF ( dtv%Cmp%k1 .NE. knd ) STOP 38
           IF ( dtv%Cmp%l1 .NE. len ) STOP 39
           IF ( SIZE(dtv%Cmp%Carr) .NE. len ) STOP 40
           IF ( SIZE(dtv%Cmp%A0) .NE.   len ) STOP 41
           IF ( SIZE(dtv%Cmp%A1) .NE. 2*len ) STOP 42
           IF ( ANY(dtv%Cmp%Carr .NE. [(CHAR(I+64), I = 1, len)]) ) STOP 43
           IF ( ANY(dtv%Cmp%A0   .NE.        [(I-1, I = 1, len)]) ) STOP 44
           IF ( ANY(dtv%Cmp%A1   .NE.      [(I+2, I = 1, 2*len)]) ) STOP 45

        CLASS DEFAULT
          STOP 46
      END SELECT

      DEALLOCATE( dtv )
END PROGRAM ExplocitInitExp02
