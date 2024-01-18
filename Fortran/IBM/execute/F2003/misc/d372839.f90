! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-12-17
!*
!*  DESCRIPTION                : defect 372839
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module foomod


  type :: foo


     integer :: intval


   contains
     procedure, pass(a) :: foosub1
     procedure, pass(a) :: foosub2
     generic, public    :: foosub => foosub1, foosub2
  end type foo


contains


  subroutine foosub1(a, b, c, d)
    class(foo)  :: a
    real         :: b
    integer      :: c
    complex, optional :: d
    print *,  'foosub1'
    return
  end subroutine foosub1


  subroutine foosub2(a, c, d, e, f)
    class(foo)  :: a
    integer      :: c
    complex, optional :: d
    real, optional :: e, f
    print *, 'foosub2'
    return
  end subroutine foosub2

end module foomod

use foomod
    type(foo) :: f1

    call f1%foosub (1.0, 1)
    call f1%foosub (1)
end
