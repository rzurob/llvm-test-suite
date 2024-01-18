!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseSameName03
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
!*  confuse the compiler by using a very similar module in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseSameName03Mod

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
     character(2) :: nm3 = "B2"
  end type base2

  type, extends (base2) :: derived2
     character(2) :: nm4 = "D2"
  end type derived2

end module dtpUseSameName03Mod

module dtpUseSameName03bMod

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

end module dtpUseSameName03bMod


program dtpUseSameName03

  use :: dtpUseSameName03bMod
  implicit none

  call mytest1
  call mytest2(4)

contains

  subroutine mytest1
    use :: dtpUseSameName03mod, only : parentClass => base
    use :: dtpUseSameName03mod, only : childClass  => derived
    implicit none

    type(parentClass)    :: p_obj
    type(childClass(2))  :: chobj

    print *, chobj, chobj % p, p_obj

  end subroutine mytest1

 
  subroutine mytest2(arg)
    use :: dtpUseSameName03mod, only : parentClass => base2
    use :: dtpUseSameName03mod, only : childClass  => derived2
    implicit none
    integer :: arg
    type(parentClass(2))   :: p_obj
    type(childClass(2))    :: chobj
    type(parentClass(arg)) :: p_obj_arg

    print *, chobj, chobj % p, p_obj, p_obj % p, p_obj_arg, p_obj_arg % p

  end subroutine mytest2
 
end program dtpUseSameName03
