!*  ===================================================================
!*
!*                               DTP - Generic Interface
!*
!*  DATE                       : October 02, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Generic Resolution - Derived-type parameters
!*  SECONDARY SUBROUTINES TESTED : Resolution for polymorphic objects (type compatible)
!*                                 based on the number of arguments
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : GENERIC
!*
!*  DESCRIPTION                :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      MODULE Mod1
      IMPLICIT NONE

      TYPE Base  (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1) :: data(l1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2)
        INTEGER, KIND :: k2

        COMPLEX(k2), ALLOCATABLE :: cmplx_data
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen(k3)
        INTEGER, KIND :: k3
      END TYPE NextGen

      INTEGER, PARAMETER :: knd1 = 4 , len1 =10
      CHARACTER(len1) :: tag

      INTERFACE SUB
         SUBROUTINE sub_no_arg
            IMPORT BASE, CHILD, NEXTGEN, KND1, LEN1
            CLASS(Base(knd1,:)), POINTER  :: pntr
         END SUBROUTINE sub_no_arg

         SUBROUTINE sub_1_arg(Obj)
            IMPORT BASE, CHILD, NEXTGEN, KND1, LEN1
            CLASS(Base(knd1,*)) :: Obj
            CLASS(Base(knd1,:)), POINTER  :: pntr
         END SUBROUTINE sub_1_arg

         SUBROUTINE sub_2_arg(Obj, Arg)
            IMPORT BASE, CHILD, NEXTGEN, KND1, LEN1
            CLASS(Base(knd1,*)) :: Obj, Arg
            CLASS(Base(knd1,:)), POINTER  :: pntr
         END SUBROUTINE sub_2_arg
      END INTERFACE

      END MODULE Mod1
!*
      SUBROUTINE sub_no_arg
         USE MOD1, Only: Base, Child, NextGen, k => knd1, l => len1, tag
         CLASS(Base(k,:)), POINTER  :: pntr

         ALLOCATE (pntr, source = Base(k1=k, l1=l)(data=(/(I*1.0, I=1, l)/)))
         IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 1

         tag = '0'

      END SUBROUTINE sub_no_arg

      SUBROUTINE sub_1_arg(Obj)
         USE MOD1, Only: Base, Child, NextGen, k => knd1, l => len1, tag
         CLASS(Base(k,*)) :: Obj
         CLASS(Base(k,:)), POINTER  :: pntr

         IF ( .NOT. ASSOCIATED(pntr)) ALLOCATE (pntr, source = Obj)
         IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 2

         tag = '1'

      END SUBROUTINE sub_1_arg

      SUBROUTINE sub_2_arg(Obj,Arg)
         USE MOD1, Only: Base, Child, NextGen, k => knd1, l => len1, tag
         CLASS(Base(k,*)) :: Obj, Arg
         CLASS(Base(k,:)), POINTER  :: pntr

         IF ( .NOT. SAME_TYPE_AS(Obj,Arg) ) THEN
            ALLOCATE (pntr,  source = Obj)
         ELSE
           SELECT TYPE (Arg)
             CLASS IS (Base(k,*))
              ALLOCATE (pntr, source = Base(k1=k, l1=l)(data=(/(1.0, I=1, l)/)))

             CLASS IS (Child(k,*,k))
              ALLOCATE (pntr, source = Child(4,5,4)(data=(/(2.0, I=1,5)/), cmplx_data=cmplx(1.0, 2.0)) )

             CLASS IS (NextGen(k,*,k,k))
              ALLOCATE (pntr, source = NextGen(4,20,4,8)(data=(/(3.0, I=1,20)/), cmplx_data=cmplx(10.0, 20.0)) )

             CLASS DEFAULT
               STOP 3
         END SELECT
         END IF

         IF ( .NOT. ASSOCIATED(pntr)) ERROR STOP 4

         tag = '2'

      END SUBROUTINE sub_2_arg
!*
      PROGRAM Generic_Interface02c
      USE MOD1
      IMPLICIT NONE

      INTEGER :: I

      CLASS(Base(knd1,:)), POINTER :: poly1
      TYPE(Base(knd1,len1))  :: base1
      TYPE(NextGen(knd1,len1,knd1,knd1)) :: dtv = NextGen(4,10,4,4)(data=(/(100.0, I=1,10)/), cmplx_data=NULL() )

      base1%data = (/(10.0, I=1, len1)/)
      CALL sub(base1)
      IF ( tag .NE. '1' ) ERROR STOP 10

      ALLOCATE(poly1, source = Base(knd1, (len1-2) )(data=(/(5.0, I=1, (len1-2))/)))
      CALL sub(poly1)
      IF ( tag .NE. '1' ) ERROR STOP 11

      CALL sub()
      IF ( tag .NE. '0' ) ERROR STOP 12

      ALLOCATE(NextGen(knd1,len1,knd1,knd1):: poly1)
      poly1%data = (/(20.0, I=1, len1)/)

      CALL sub(poly1)
      IF ( tag .NE. '1' ) ERROR STOP 13
      CALL sub(poly1,base1)
      IF ( tag .NE. '2' ) ERROR STOP 14
      CALL sub(poly1,poly1)
      IF ( tag .NE. '2' ) ERROR STOP 15

      ALLOCATE(NextGen(knd1,2*len1,knd1+knd1,2*knd1):: poly1)
      poly1%data = (/(30.0, I=1, len1)/)
      CALL sub(poly1)
      IF ( tag .NE. '1' ) ERROR STOP 16

      CALL sub()
      IF ( tag .NE. '0' ) ERROR STOP 17

      CALL sub(dtv,base1)
      IF ( tag .NE. '2' ) ERROR STOP 18
      CALL sub(dtv,poly1)
      IF ( tag .NE. '2' ) ERROR STOP 19
      CALL sub(dtv,dtv)
      IF ( tag .NE. '2' ) ERROR STOP 20

      END PROGRAM Generic_Interface02c
