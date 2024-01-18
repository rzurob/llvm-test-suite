      MODULE Mod1

      TYPE Base  (l1)
        INTEGER, LEN :: l1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen
        CLASS(Base(l1)), ALLOCATABLE :: cmp
      END TYPE NextGen

      CONTAINS
!*
      SUBROUTINE sub2(Obj,Arg,poly)  !<-- If Arg is removed the tc has the right behavior
      CLASS(Child(*)) :: Obj
      CLASS(Base(*)) :: Arg
      CLASS(Base(:)), ALLOCATABLE :: poly

      ALLOCATE (poly, source = Obj)
      IF ( .NOT. ALLOCATED(poly)) STOP 101

      END SUBROUTINE sub2

      END MODULE Mod1
!*
      PROGRAM Generic_TypeBound02d
      USE MOD1

      TYPE(child(10)) :: tgt1
      TYPE(NextGen(10)) :: dtv
      !CLASS(Base(:)), POINTER :: pntr  !<-- If this line is added the tc has the right behavior

      call sub2(tgt1,tgt1,dtv%cmp)

     END PROGRAM Generic_TypeBound02d
