!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-09-08
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : USE in contained context
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that two types which are not known at all in the host but known by
!*  the same name in two different contained contexts work correctly
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseSameName02mod

  implicit none

  type base
     character(2) :: nm1 = "B0"
  end type base

  type, extends (base) :: derived(p)
     integer, kind :: p
     character(2) :: nm2 = "D0"
  end type derived


  type base2 (p)
    integer, len :: p
     character(p) :: nm3 = "B2BBBBBBB" ! depending on 'p', this should be B, B2, B2B, etc.
  end type base2

  type, extends (base2) :: derived2
     character(p) :: nm4 = "D2DDDDDDD" ! depending on 'p', this should be D, D2, D2D, etc.
  end type derived2

end module dtpUseSameName02mod


program dtpUseSameName02

  implicit none

  call mytest1

contains

  subroutine mytest1
    use :: dtpUseSameName02mod, only : parentClass => base
    use :: dtpUseSameName02mod, only : childClass  => derived
    implicit none

    type(parentClass)    :: p_obj
    type(childClass(2))  :: chobj

    call mytest2(p_obj, chobj, 4)

  end subroutine mytest1


  subroutine mytest2(arg1,arg2,arg3)
    use :: dtpUseSameName02mod, only : parentClass => base2
    use :: dtpUseSameName02mod, only : childClass  => derived2
    use :: dtpUseSameName02mod, only : pc => base
    use :: dtpUseSameName02mod, only : cc => derived
    implicit none
    type(pc) :: arg1
    type(cc(2)) :: arg2
    integer :: arg3
    type(parentClass(2))   :: p_obj
    type(childClass(2))    :: chobj
    type(parentClass(arg3)) :: p_obj_arg
    type(parentClass(arg2%p+1)) :: p_obj_arg2

    print *, chobj, chobj % p, p_obj, p_obj % p, p_obj_arg, p_obj_arg % p, p_obj_arg2, p_obj_arg2 % p
           ! B2D2   2          B2     2          B2BB       4              B2B         3
    print *, arg1, arg2, arg2 % p, arg3
           ! B0    B0D0  2         4 (no space between B0 and B0D0, actually)
    call mytest3(p_obj, chobj, p_obj_arg, p_obj_arg2)

  end subroutine mytest2

  subroutine mytest3(arg1,arg2,arg3,arg4)
    use :: dtpUseSameName02mod, only : parentClass => base
    use :: dtpUseSameName02mod, only : childClass  => derived
    use :: dtpUseSameName02mod, only : pc => base2
    use :: dtpUseSameName02mod, only : cc => derived2
    implicit none
    integer :: arg
    type(pc(*)) :: arg1, arg3, arg4
    type(cc(*)) :: arg2
    type(childClass(2)) :: chobj
    type(parentClass)   :: p_obj

    print *, chobj, chobj % p, p_obj
           ! B0D0   2          B0
    print *, arg1, arg1 % p, arg2, arg2 % p, arg3, arg3 % p, arg4, arg4 % p
           ! B2    2         B2D2  2         B2BB  4         B2B   3
  end subroutine mytest3

end program dtpUseSameName02