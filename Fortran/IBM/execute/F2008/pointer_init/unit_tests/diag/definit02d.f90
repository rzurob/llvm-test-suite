! Pointer initialization langlvl checking.
subroutine sub
  implicit none
  abstract interface
    integer function foo(i)
      integer, value :: i
    end function
  end interface
  procedure(foo) bar

  integer, target, save :: t1 

  type dt1
    integer, pointer :: p1 => t1
    integer, pointer :: p2 => null()
    procedure(foo), pointer, nopass :: pp1 => bar
    procedure(foo), pointer, nopass :: pp2 => null()
  end type

  integer, pointer :: p3 => t1
  integer, pointer :: p4 => null()
  procedure(foo), pointer :: pp3 => bar
  procedure(foo), pointer :: pp4 => null()
end subroutine
