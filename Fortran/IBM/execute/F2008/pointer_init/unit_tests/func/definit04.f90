! derived type definition with pointer initialization.
! Derived type definition is use-associated
!  - Pointer init target is use-associated and renamed
!  - Test different types of designators:
!      object name, array element, array section, complex part,
!      structure component, substring
module m
  implicit none
  complex, target, save :: t(3) = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
  type s
    character(5) c
  end type
  type(s), target :: ts(2) = [s('ABCDE'), s('FGHIJ')]
  type dt
    complex, pointer :: p1(:) => t
    complex, pointer :: p2 => t(3)
    complex, pointer :: p3a(:) => t(2:3)
    complex, pointer :: p3(:) => t(1:3:2)
    real, pointer :: p4 => t(3)%im
    real, pointer :: p5(:) => t%re
    character(5), pointer :: p6(:) => ts%c
    character(2), pointer :: p6a(:) => ts%c(2:3)
  end type
end module

subroutine sub
  use m
  implicit none
  type(dt) d
  if (.not. associated(d%p1, t)) error stop 1
  if (.not. associated(d%p2, t(3))) error stop 2
  if (.not. associated(d%p3a, t(2:3))) error stop 3
  if (.not. associated(d%p3, t(1:3:2))) error stop 5
  if (.not. associated(d%p4, t(3)%im)) error stop 5
  if (.not. associated(d%p5)) error stop 6
  if (any(d%p5 /= [1.0, 3.0, 5.0])) error stop 7
  if (.not. associated(d%p6, ts%c)) error stop 8
  if (.not. associated(d%p6a, ts%c(2:3))) error stop 9
  if (any(d%p6 /= ts%c)) error stop 10
  if (any(d%p6a /= ['BC', 'GH'])) error stop 11
end subroutine

program main
  call sub
end program
