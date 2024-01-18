!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 19, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Namelist with Intrinsic IO
!*  SECONDARY FUNCTIONS TESTED : Namelist-group-name appear more than once in the
!*                               NAMELIST statements in a scoping unit
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

      TYPE, EXTENDS(Base) :: Child2 (k2,l2)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        CHARACTER(l1+l2) :: name = 'XLF'
      END TYPE Child2

      TYPE, EXTENDS(Base) :: Child3 (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3

        CHARACTER(l1+l3) :: name = 'XLF'
      END TYPE Child3

      TYPE, EXTENDS(Child2) :: NextGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4

        TYPE(Child2(k1,l1,k4,l4)) :: c2_cmp(l4)
        TYPE(Child3(k1,l1,k4,l4)) :: c3_cmp(l4)
      END TYPE NextGen

      END MODULE Mod1
!*
      PROGRAM Namelist14
       USE MOD1
       IMPLICIT NONE

       TYPE(Base(4,3)) :: b1 = Base(4,3) (val=-11)
       TYPE(Child2(4,3,4,1)) :: c2 = Child2(4,3,4,1)(val=-22)
       TYPE(Child3(4,3,4,1)) :: c3 = Child3(4,3,4,1)(val=-33)
       TYPE(NextGen(4,3,5,5,4,1)), ALLOCATABLE :: n1(:,:,:,:,:,:,:)

       CHARACTER(100) :: string, msg
       INTEGER :: stat

       NAMELIST /NMLb/b1
       NAMELIST /NMLc/c2
       NAMELIST /NMLc/c3
       NAMELIST /NMLn/n1
       NAMELIST /NMLn/b1,c2,c3,n1

       OPEN (14, file = 'Namelist14.In', form='formatted')

       READ(14, NML=NMLb, iostat = stat, iomsg = msg)
       IF ( stat .NE. 0 ) STOP 100
       READ(14, NML=NMLc, iostat = stat, iomsg = msg)
       IF ( stat .NE. 0 ) STOP 101

       IF (b1%val .NE. -1) STOP 10
       IF (c2%val .NE. -2) STOP 11
       IF (c3%val .NE. -3) STOP 12
       IF (c2%name .NE. 'ABCD') STOP 13
       IF (c3%name .NE. 'GHIJ') STOP 14

       !WRITE(*, NML=NMLb, iostat = stat, iomsg = msg)
       !WRITE(*, NML=NMLc, iostat = stat, iomsg = msg)

       ALLOCATE(n1(1,1,1,1,1,1,2))

       READ(14, NML=NMLn, iostat = stat, iomsg = msg)

       IF (n1(1,1,1,1,1,1,2)%val .NE. 99) STOP 15
       IF (n1(1,1,1,1,1,1,2)%name .NE. 'Erwin Ru') STOP 16

       IF (n1(1,1,1,1,1,1,1)%val .NE. 99) STOP 17
       IF (n1(1,1,1,1,1,1,1)%name .NE. 'Henrik D') STOP 18

       IF (n1(1,1,1,1,1,1,1)%c2_cmp(1)%val .NE. c2%val) STOP 15
       IF (n1(1,1,1,1,1,1,1)%c2_cmp(1)%name .NE. c2%name) STOP 16
       IF (n1(1,1,1,1,1,1,2)%c2_cmp(1)%val .NE. c2%val) STOP 17
       IF (n1(1,1,1,1,1,1,2)%c2_cmp(1)%name .NE. c2%name) STOP 18

       IF (n1(1,1,1,1,1,1,1)%c3_cmp(1)%val .NE. c3%val) STOP 18
       IF (n1(1,1,1,1,1,1,1)%c3_cmp(1)%name .NE. c3%name) STOP 19
       IF (n1(1,1,1,1,1,1,2)%c3_cmp(1)%val .NE. c3%val) STOP 20
       IF (n1(1,1,1,1,1,1,2)%c3_cmp(1)%name .NE. c3%name) STOP 21

       DEALLOCATE(n1)

      END PROGRAM Namelist14
