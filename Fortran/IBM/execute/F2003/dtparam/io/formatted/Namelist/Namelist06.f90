!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 17, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : None
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

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER :: val = 10
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,k3,l2,l3)
        INTEGER, KIND :: k2, k3
        INTEGER, LEN  :: l2, l3

      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4

        INTEGER(k1)   :: Iarr(l4)
        CHARACTER(l1+l2+l3+l4) :: name = 'ABCDEF'
        TYPE(Base(k1,l1)) :: b_cmp(l1)
        TYPE(Child(k1,l1,k2,k3,l2,l3)) :: c_cmp(l2+l3)
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist06
       USE MOD1
       IMPLICIT NONE

       TYPE(Base(4,:)), POINTER :: b1(:,:,:,:,:,:,:)
       TYPE(Child(4,3,4,10,4,10)) :: c1 = Child(4,3,4,10,4,10)(val=-1)
       TYPE(NextGen(4,3,4,4,5,5,20,20)), ALLOCATABLE :: n1(:,:,:,:,:,:,:)

       CHARACTER(100) :: string, msg
       INTEGER :: stat

       NAMELIST /NMLb/b1, /NMLc/c1, /NMLn/n1
       NAMELIST /NMLt/b1,c1,n1,b1

       ALLOCATE (Base(4,10) :: b1(2,2,2,2,2,2,2))

       WRITE(*, NML=NMLb, iostat = stat, iomsg = msg)
       WRITE(*, NML=NMLc, iostat = stat, iomsg = msg)

       ALLOCATE(n1(1,1,1,1,1,1,1))

       n1 = NextGen(4,3,4,4,5,5,20,20) ( val = 1, &
          & c_cmp = Child(4,3,4,4,5,5) (val = 99) , b_cmp = b1(:,1,1,1,1,1,1), &
          & Iarr = 1.5, name = 'Erwin Rudolf Josef Alexander Schrodinger' )


       WRITE(*, NML=NMLn, iostat = stat, iomsg = msg)

       string = 'Niels Henrik David Bohr'
       n1%name = string

       WRITE(*, NML=NMLt, iostat = stat, iomsg = msg)

       DEALLOCATE(b1,n1)

      END PROGRAM Namelist06
