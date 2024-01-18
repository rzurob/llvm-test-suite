!*  ===================================================================
!*
!*                               DTP-SELECT TYPE Construct
!*
!*  DATE                       : August 26, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SELECT TYPE Construct - Derived-type parameters
!*  SECONDARY FUNCTIONS TESTED : Use association
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!*  R823 type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS IS ( type-spec ) [ select-construct-name ]
!*                       or CLASS DEFAULT [ select-construct-name ]
!*
!* See defect 355714
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(LEN=l1) :: Ctext(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(k1,l1)), POINTER :: Cmp
      END TYPE Child

      INTEGER, PARAMETER :: knd1 = 4, len1 = 10
      INTEGER :: I

      CONTAINS

      SUBROUTINE sub1(Arg)
      CLASS(*), POINTER, INTENT(INOUT) :: Arg
      TYPE(Base(k1=knd1,l1=len1)), TARGET :: Tgt = (Base(knd1,len1) (Ctext=(/ ('', I = 1, len1) /)) )
      TYPE(Base(k1=knd1,l1=len1)), TARGET :: cbl = (Base(knd1,len1) (Ctext=(/ ('BBB', I = 1, len1) /)) )  !cbl has the SAVE attribute

      ALLOCATE( Arg, SOURCE = ( Child(k1=knd1,l1=len1) (Ctext=(/ ('AAA', I = 1, len1) /), Cmp = Tgt) ) )

      IF ( .NOT. ASSOCIATED(Arg) ) STOP 10

      SELECT TYPE ( A => Arg )
        CLASS IS (Child(knd1,*))
           A%Ctext=(/ 'I am not c', 'ertain wha', 't the lang', 'uage for s', 'cientific ' ,'computatio', &
              & 'n will loo', 'k like by ', 'the 21st c', 'entury... ' /)
           IF ( .NOT. ASSOCIATED(A%Cmp, Tgt ) ) STOP 11
           A%Cmp => cbl  ! pointer re-assignment

        CLASS IS (Base(knd1,*))
          STOP 12

        CLASS DEFAULT
          STOP 13
      END SELECT


      END SUBROUTINE sub1

END MODULE Mod1
!*
PROGRAM Select_Type03b
      USE Mod1
      IMPLICIT NONE

      CLASS(*), POINTER :: dtv

      CALL sub1(dtv)

      SELECT TYPE ( dtv )
        CLASS IS (Child(knd1,*))
          print*, dtv%Ctext
          IF ( ANY(dtv%Cmp%Ctext .NE. 'BBB') ) STOP 20

        CLASS IS (Base(knd1,*))
          STOP 21

        CLASS DEFAULT
          STOP 22
      END SELECT

      DEALLOCATE( dtv )

END PROGRAM Select_Type03b
