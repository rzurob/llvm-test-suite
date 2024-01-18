!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseSameName04
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-09-08
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : USE in contained context
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that two types which are not known at all in the host but known by
!*  the same name in two different contained contexts work correctly.  Try to
!*  confuse the compiler by using a very similar module in the main program,
!*  and use ONLY and/or rename.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseSameName04Mod

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

end module dtpUseSameName04Mod

module dtpUseSameName04bMod

  implicit none

  type base
     character(2) :: nm1 = "B0"
  end type base

  type, extends (base) :: derived(p)
     integer, kind :: p
     real :: r = 3.1415
     character(2) :: nm2 = "D0"
  end type derived


  type base2 (p)
    integer, len :: p
     real :: q = 2.718
     character(p) :: nm3 = "X2xxxxxx"
  end type base2

  type, extends (base2) :: derived2
     character(p) :: nm4 = "Y2yyyyyy"
  end type derived2

end module dtpUseSameName04bMod


program dtpUseSameName04

  use :: dtpUseSameName04bMod, only: parentClass => derived
  use :: dtpUseSameName04bMod, only: childClass => base
  implicit none

  call mytest1

contains

  subroutine mytest1
    use :: dtpUseSameName04mod, only : parentClass => base
    use :: dtpUseSameName04mod, only : childClass  => derived
    implicit none

    type(parentClass)    :: p_obj
    type(childClass(2))  :: chobj

    call mytest2(p_obj, chobj, 4)

  end subroutine mytest1


  subroutine mytest2(arg1,arg2,arg3)
    use :: dtpUseSameName04mod, only : parentClass => base2
    use :: dtpUseSameName04mod, only : childClass  => derived2
    use :: dtpUseSameName04mod, only : pc => base
    use :: dtpUseSameName04mod, only : cc => derived
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
    use :: dtpUseSameName04mod, only : parentClass => base
    use :: dtpUseSameName04mod, only : childClass  => derived
    use :: dtpUseSameName04mod, only : pc => base2
    use :: dtpUseSameName04mod, only : cc => derived2
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
 
end program dtpUseSameName04
