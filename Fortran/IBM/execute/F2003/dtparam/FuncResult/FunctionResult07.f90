!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : FunctionResult07.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : March 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Function result - unlimited poly         
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
!* Defect:  362586
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod 
      IMPLICIT NONE

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1) :: A0(l1)
        CHARACTER(l1) :: name 
        INTEGER(k1) :: A1(l1)
      END TYPE

      TYPE,  EXTENDS(Base) :: Child (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2+2)) :: b_cmp
      END TYPE

      CONTAINS

      FUNCTION foo(Arg)
        CLASS(Base(4,:)), POINTER :: Arg 
        CLASS(Base(4,:)), POINTER :: foo
 
        foo => Arg
 
      END FUNCTION   
END MODULE
PROGRAM FunctionResult07
      USE Mod
      IMPLICIT NONE

      CLASS(base(4,:)), POINTER :: b1, b2
      TYPE(child(4,5,4,10)), TARGET :: c1

      c1%name = 'IBM'
      c1%A0 = 1
      c1%A1 = 2
      c1%b_cmp = ( Base(4,12) ( 3, 'XLF', 4) )

      b1 => c1 
      SELECT TYPE ( b1 )
        CLASS IS (Child(4,*,4,*))
          IF ( b1%name   .NE. 'IBM' ) STOP 10
          IF ( ANY(b1%A0 .NE.    1) ) STOP 11
          IF ( ANY(b1%A1 .NE.    2) ) STOP 12
          IF ( b1%b_cmp%name   .NE. 'XLF' ) STOP 13
          IF ( ANY(b1%b_cmp%A0 .NE.    3) ) STOP 14
          IF ( ANY(b1%b_cmp%A1 .NE.    4) ) STOP 15

        CLASS DEFAULT
           STOP 16
      END SELECT

      b2 => foo (b1)
      if ( .NOT. ASSOCIATED(b2, b1) ) STOP 17
      SELECT TYPE ( b2 )
        CLASS IS (Child(4,*,4,*))
          IF ( b2%name   .NE. 'IBM' ) STOP 18
          IF ( ANY(b2%A0 .NE.    1) ) STOP 19
          IF ( ANY(b2%A1 .NE.    2) ) STOP 20
          IF ( b2%b_cmp%name   .NE. 'XLF' ) STOP 21
          IF ( ANY(b2%b_cmp%A0 .NE.    3) ) STOP 22
          IF ( ANY(b2%b_cmp%A1 .NE.    4) ) STOP 23

        CLASS DEFAULT
           STOP 24
      END SELECT

      ASSOCIATE ( a => foo (b2) ) 
        SELECT TYPE ( a )
          CLASS IS (Child(4,*,4,*))
            IF ( a%name   .NE. 'IBM' ) STOP 25
            IF ( ANY(a%A0 .NE.    1) ) STOP 26
            IF ( ANY(a%A1 .NE.    2) ) STOP 27
            IF ( a%b_cmp%name   .NE. 'XLF' ) STOP 28
            IF ( ANY(a%b_cmp%A0 .NE.    3) ) STOP 29
            IF ( ANY(a%b_cmp%A1 .NE.    4) ) STOP 30

          CLASS DEFAULT
             STOP 31
        END SELECT
      END ASSOCIATE 
END PROGRAM FunctionResult07
