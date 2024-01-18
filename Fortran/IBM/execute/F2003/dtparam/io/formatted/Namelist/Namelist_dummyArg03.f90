!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONMLtY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Namelist_dummyArg03
!*                               DTP - Namelist
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : November 20, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : None
!*                     
!*
!*  DRIVER STANZA              : xlf2003
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
!*
!* defect 359344
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE 

      TYPE Base (k1,l1)  ! (4,10)
        INTEGER, KIND :: k1 
        INTEGER, LEN  :: l1 

        CONTAINS
        PROCEDURE :: cfoo                 
      END TYPE Base 

      TYPE, EXTENDS(Base) :: Child (k2,l2)  ! (4,10)
        INTEGER, KIND :: k2 
        INTEGER, LEN  :: l2 

        REAL(k2) :: Rarr(l1) = k2
      END TYPE Child 

      TYPE, EXTENDS(Child) :: NextGen (k3,l3) ! (5,5)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3
 
        CHARACTER(l2) :: Carr(l2) = 'ABCD '
        INTEGER(k2)   :: Iarr(l2) = k3
      END TYPE NextGen

      CONTAINS
 
      FUNCTION cfoo(Arg)
       CLASS(Base(4,*)) :: Arg
       COMPLEX cfoo 
         cfoo = (Arg%k1,Arg%l1)
      END FUNCTION cfoo
 
END MODULE Mod1
!*
PROGRAM Namelist_dummyArg03
      USE MOD1
      IMPLICIT NONE
 
       TYPE(Base(4,:)), POINTER :: b1(:)
       TYPE(Child(4,10,4,10)) :: c1 = Child(4,10,4,10)(Rarr = -1)
       TYPE(NextGen(4,10,4,10,5,5)), ALLOCATABLE :: n1(:)

       CHARACTER(100) :: string

       NAMELIST /NMLb/b1, /NMLc/c1, /NMLn/n1

       ALLOCATE (Base(4,10) :: b1(2))

       string = 'Niels'

       ALLOCATE(n1(2), source = NextGen(4,10,4,10,5,5) (Rarr = 9.5, Carr = string, Iarr = 1))

       print NMLb
       print NMLc
       print NMLn

       call sub(b1,c1,n1)

       DEALLOCATE(b1,n1)

      CONTAINS
!*
      SUBROUTINE sub(argb1, argc1, argn1)
       TYPE(Base(4,10)), INTENT(INOUT)             :: argb1(:)
       TYPE(Child(4,10,4,10)), INTENT(INOUT)       :: argc1
       TYPE(NextGen(4,10,4,10,5,5)), INTENT(INOUT) :: argn1(:)

       NAMELIST /NMLb/argb1, /NMLc/argc1, /NMLn/argn1
 
       argc1%Rarr = 88.3
       argn1 = NextGen(4,10,4,10,5,5) (66.22, 'Erwin', 5) 

       WRITE(*, NML=NMLb)
       WRITE(*, NML=NMLc)
       WRITE(*, NML=NMLn)

      END SUBROUTINE sub

END PROGRAM Namelist_dummyArg03
