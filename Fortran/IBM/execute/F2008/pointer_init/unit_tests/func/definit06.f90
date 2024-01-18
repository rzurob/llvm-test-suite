! derived type definition with pointer initialization.
!  - Test types with multiple pointers and several levels deep
module m
  implicit none
  complex, target, save :: t(3) = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
  type s
    character(5) c
  end type
  type(s), target :: ts(2) = [s('ABCDE'), s('FGHIJ')]

  type level3
    integer i
    real, pointer :: p4 => t(3)%im
    real, pointer :: p5(:) => t%re
  end type

  type(level3), target :: l3_obj

  type level2
    integer j
    complex, pointer :: p3(:) => t(1:3:2)
    type(level3) :: l3(2)
    character(5), pointer :: p6(:) => ts%c
    type(level3), pointer :: l3b => l3_obj
  end type

  type level1
    complex, pointer :: p1(:) => t
    complex, pointer :: p2 => t(3)
    complex, pointer :: p3a(:) => t(2:3)
    character(2), pointer :: p6a(:) => ts%c(2:3)
    type(level2) l2
  end type
end module

subroutine sub
  use m
  implicit none
  type(level1) d
  if (.not. associated(d%p1, t)) error stop 1
  if (.not. associated(d%p2, t(3))) error stop 2
  if (.not. associated(d%p3a, t(2:3))) error stop 3
  if (.not. associated(d%l2%p3, t(1:3:2))) error stop 5
  if (.not. associated(d%l2%l3(1)%p4, t(3)%im)) error stop 6
  if (.not. associated(d%l2%l3(2)%p4, t(3)%im)) error stop 7
  if (.not. associated(d%l2%l3(1)%p5)) error stop 8
  if (.not. associated(d%l2%l3(2)%p5)) error stop 9
  if (any(d%l2%l3(1)%p5 /= [1.0, 3.0, 5.0])) error stop 10
  if (any(d%l2%l3(2)%p5 /= [1.0, 3.0, 5.0])) error stop 11
  if (.not. associated(d%l2%p6, ts%c)) error stop 9
  if (.not. associated(d%p6a, ts%c(2:3))) error stop 10
  if (any(d%l2%p6 /= ts%c)) error stop 11
  if (any(d%p6a /= ['BC', 'GH'])) error stop 12
  if (.not. associated(d%l2%l3b, l3_obj)) error stop 13
end subroutine

subroutine sub2
  use m
  implicit none
  type(level1), save :: d2
  if (.not. associated(d2%p1, t)) error stop 101
  if (.not. associated(d2%p2, t(3))) error stop 102
  if (.not. associated(d2%p3a, t(2:3))) error stop 103
  if (.not. associated(d2%l2%p3, t(1:3:2))) error stop 105
  if (.not. associated(d2%l2%l3(1)%p4, t(3)%im)) error stop 106
  if (.not. associated(d2%l2%l3(2)%p4, t(3)%im)) error stop 107
  if (.not. associated(d2%l2%l3(1)%p5)) error stop 108
  if (.not. associated(d2%l2%l3(2)%p5)) error stop 109
  if (any(d2%l2%l3(1)%p5 /= [1.0, 3.0, 5.0])) error stop 110
  if (any(d2%l2%l3(2)%p5 /= [1.0, 3.0, 5.0])) error stop 111
  if (.not. associated(d2%l2%p6, ts%c)) error stop 112
  if (.not. associated(d2%p6a, ts%c(2:3))) error stop 113
  if (any(d2%l2%p6 /= ts%c)) error stop 114
  if (any(d2%p6a /= ['BC', 'GH'])) error stop 115
  if (.not. associated(d2%l2%l3b, l3_obj)) error stop 116
end subroutine

program main
  call sub
  call sub2
end program
