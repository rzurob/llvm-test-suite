

program main
  type t
    class(*), allocatable:: x
  end type t

  type t2
    integer:: j=4
  end type t2

  type(t) :: arr(10)
  class(*), target, allocatable :: x1
  class(*), pointer :: p
  integer :: i
  do i=1,10,1
    arr(i)%x = t2(i)
    print *, allocated (arr(i)%x)
    x1 = arr(i)%x
    p=>x1
    select type (p)
      type is (t2)
        print *, p%j
    end select
    !print *, arr(i)%x
  end do

end
