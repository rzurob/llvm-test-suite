!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 25, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Namelist with Intrinsic IO
!*  SECONDARY SUBROUTINES TESTED : Dummy argument with deferred LEN parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : NAMELIST
!*
!*  DESCRIPTION                :
!*  namelist-stmt is
!*
!*     NAMELIST  / namelist-group-name / namelist-group-object-list &
!*     [ [ , ] / namelist-group-name / namelist-group-object-list ] . . .
!*
!* see 359419
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)  !(4,10)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1/k1) :: Iarr(l1+1-5) = k1
        CHARACTER(l1)  :: Carr(l1) = 'ABCD '

        CONTAINS
        PROCEDURE :: fsingle => sfoo
        GENERIC   :: init => fsingle
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)  !(4,10)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2)) :: dtBarr(l2/l1)
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k3,k4,l3,l4) !(4,4,10,10)
        INTEGER, KIND :: k3, k4
        INTEGER, LEN  :: l3, l4

        TYPE(Child(k3,l3,k4,l4)) :: dtCarr(l3/l4)
      END TYPE NextGen

      CONTAINS

      SUBROUTINE sfoo(Arg)
         CLASS(Base(4,*)) :: Arg

      END SUBROUTINE sfoo

END MODULE Mod1
!*
PROGRAM Namelist_dummyArg06
      USE MOD1
       IMPLICIT NONE

       INTEGER(1), PARAMETER :: single = 4, length = 10

       TYPE(Base(single,length)) :: b1
       TYPE(Child(single,length,single,length)) :: c1, c2
       POINTER :: c2

       NAMELIST /NMLb/b1, /NMLc/c1
       NAMELIST /NMLt/b1, c2

       b1 = Base(single,length) (Carr = 'Werner', Iarr = 1)

       c1 = Child(single,length,single,length) (1 , 'Niels', &
            & dtBarr = Base(single,length) (Carr = 'Erwin', Iarr = 6) )

       c2 => func(b1)

       WRITE(*, NML=NMLb, DELIM='QUOTE')
       WRITE(*, NML=NMLc, DELIM='QUOTE')
       WRITE(*, NML=NMLt, DELIM='QUOTE')

      CONTAINS
!*
      FUNCTION func(barg)
       TYPE(Base(single,*)) :: barg
       TYPE(Child(single,:,single,:)), POINTER :: func


       NAMELIST /NMLb/barg, /NMLf/func

       ALLOCATE(func, source = Child(single,barg%l1,single,barg%l1) (Carr = 'TEST', &
              & dtBarr = barg, Iarr = 99) )

       WRITE(*, NML=NMLb, DELIM='QUOTE')
       WRITE(*, NML=NMLf, DELIM='QUOTE')

      END FUNCTION func

END PROGRAM Namelist_dummyArg06
