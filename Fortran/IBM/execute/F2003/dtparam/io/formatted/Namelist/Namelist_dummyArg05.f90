!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY SUBROUTINES TESTED   : Namelist with Intrinsic IO
!*  SECONDARY SUBROUTINES TESTED : None
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
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)  !(4,10)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1/k1) :: Iarr(l1+1-5) = k1
        COMPLEX(k1)    :: Zarr(l1/5)   = (k1,k1)
        CHARACTER(l1)  :: Carr(l1) = 'ABCD '

        CONTAINS
        PROCEDURE :: fsingle => sfoo
        PROCEDURE :: fdouble => dfoo
        GENERIC   :: init => fsingle, fdouble
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)  !(4,10)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2)) :: dtarr(l2/l1)
      END TYPE Child

      INTEGER(1), PARAMETER :: single = 4, double = 8

      CONTAINS

      SUBROUTINE sfoo(Arg)
         CLASS(Base(single,*)) :: Arg

       SELECT TYPE (Arg)
          TYPE IS (Base(single,*))
             Arg = Base(single,Arg%l1) (Zarr = (1.1,1.1) , Carr = 'Sfoo ', Iarr = 1)

          CLASS DEFAULT
           STOP 10
      END SELECT

      END SUBROUTINE sfoo

      SUBROUTINE dfoo(Arg)
         CLASS(Base(double,*)) :: Arg

       SELECT TYPE (Arg)
          TYPE IS (Base(double,*))
             Arg = Base(double,Arg%l1) (Zarr = (2.2,2.2) , Carr = 'Dfoo ', Iarr = 2)

          CLASS DEFAULT
           STOP 11
      END SELECT

      END SUBROUTINE dfoo

END MODULE Mod1
!*
PROGRAM Namelist_dummyArg05
      USE MOD1

       IMPLICIT TYPE(Base(4,10)) (b)
       IMPLICIT TYPE(Child(4,10,4,10)) (c)

       NAMELIST /NMLb/b1, /NMLc/c1
       NAMELIST /NMLb/b2, /NMLc/c2

       c1 = Child(4,10,4,10) (Zarr = (9.5,9.5) , Carr = 'Niels', Iarr = 1, &
            & dtarr = Base(4,10) (Zarr = (6.6,6.6) , Carr = 'Erwin', Iarr = 6) )

       print NMLb
       print NMLc

       call b2%init ()

       print NMLb
       print NMLc

       call sub1(b1,c1)

       call sub2()

      CONTAINS
!*
      SUBROUTINE sub2

       NAMELIST /NMLb/b1, /NMLc/c1 ! host association

       WRITE(*, NML=NMLb)
       WRITE(*, NML=NMLc)

      END SUBROUTINE sub2

      SUBROUTINE sub1(barg, carg)

       NAMELIST /NMLb/barg, /NMLc/carg ! argument association

       WRITE(*, NML=NMLb)
       WRITE(*, NML=NMLc)

      END SUBROUTINE sub1

END PROGRAM Namelist_dummyArg05
