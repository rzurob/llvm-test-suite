! derived type definition with pointer initialization.
! Diagnostics
subroutine sub
  implicit none
  integer, target :: t1 
  real, target, save :: t2(2) = [1.0, 2.0]
  integer :: t3
  integer, target, save, allocatable :: t4

  type dt
    integer :: i = 3
    integer, pointer :: p1 => t1    ! error 1 
    integer, pointer :: p2 => t2(2) ! error 2
    integer, pointer :: p3 => t3    ! error 3
    integer, pointer :: p4 => t4    ! error 4
    real, pointer :: p5(:) => t2([1,1]) ! error 5
    real, pointer :: p6 => t2(t3)   ! error 6
  end type

  type dt1
    integer, pointer :: p1 => t1    ! error 7: target not saved
  end type

  type dt2
    integer, pointer :: p2 => t2(1) ! error 8: wrong type
  end type

  type dt2a
    real, pointer :: p2 => t2       ! error 9: wrong rank
  end type

  type dt3
    integer, pointer :: p3 => t3    ! error 10: missing target attr
  end type

  type dt4
    integer, pointer :: p4 => t4    ! error 11: target is allocatable
  end type

  type dt5
    real, pointer :: p5 => t2([1,1])! error 12: target has vector subscript
  end type
end subroutine
