! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               scalars
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

class(*), allocatable, target :: a
class(*), allocatable, target :: b
class(*), pointer :: p

allocate(a, source=5_8)
select type (a)
  type is (integer(8))
    if (a /= 5_8) error stop 1
  class default
    stop 2
end select

p => a
select type (p)
  type is (integer(8))
    if (p /= 5_8) error stop 3
  class default
    stop 4
end select

allocate(b, source=100.4_16)
select type (b)
  type is (real(16))
    if (b /= 100.4_16) error stop 5
  class default
    stop 6
end select

call move_alloc(a,b)
if (allocated(a)) error stop 7
if (.not.allocated(b)) error stop 8
if (.not.associated(p,b)) error stop 9

select type (b)
  type is (integer(8))
    if (b /= 5_8) error stop 10
  class default
    stop 11
end select

select type (p)
  type is (integer(8))
    if (p /= 5_8) error stop 12
  class default
    stop 13
end select

end