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
!* see defec 359604
!*
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE, ABSTRACT :: Base (k1,l1)  !(4,10)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1/k1) :: Iarr(l1+1-5) = k1
        CHARACTER(l1)  :: Carr(l1) = 'ABCD'

        CONTAINS
        PROCEDURE :: fsingle => sfoo
        GENERIC   :: init => fsingle
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)  !(4,10)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2
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
PROGRAM Namelist_dummyArg09
      USE MOD1
       IMPLICIT NONE

       INTEGER(1), PARAMETER :: single = 4, length = 10

       TYPE(NextGen(single,length,single,length,single,single,length,length)) :: n1
       TYPE(Child(single,length,single,length)) :: c1

       NAMELIST /NMLb/n1, /NMLc/c1
       NAMELIST /NMLt/n1,c1

       OPEN (1, file = 'Namelist_dummyArg09.In', form='formatted')

       call sub(n1)

       READ(1, NML=NMLb)
       READ(1, NML=NMLc)

       WRITE(*, NML=NMLt, DELIM='QUOTE')

      CONTAINS
!*
     SUBROUTINE sub(Arg)
       TYPE(NextGen(single,*,single,*,single,single,*,*)) :: Arg
       TYPE(NextGen(single,:,single,:,single,single,:,:)), POINTER :: ptr

       NAMELIST /NMLb/Arg, /NMLp/ptr
       NAMELIST /NMLt/Arg,ptr

       ALLOCATE(NextGen(single,length,single,length,single,single,length,length) :: ptr)

       READ(1, NML=NMLb)
       READ(1, NML=NMLp)

       WRITE(*, NML=NMLt, DELIM='QUOTE')

      END SUBROUTINE sub

END PROGRAM Namelist_dummyArg09
