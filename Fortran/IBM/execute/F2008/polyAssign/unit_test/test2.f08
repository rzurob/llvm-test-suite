
program main
  type t
    class(*), allocatable:: x
  end type t

  type :: t2
    integer:: j=4
    integer:: k=4
    integer:: l=4
    integer:: m=4
  end type t2

  type(t) :: arr(10)
  class(*), target, allocatable :: x1
  class(*), pointer :: p
  integer :: i
  do i=1,10,1
    x1 = retPoly(p)
    !if (allocated(x1)) then
    !  deallocate(x1)
    !end if
    !allocate (x1, source = retPoly(p))
    !print *, "storage size x1"
    !print *, storage_size(x1)
    arr(i)%x = t2(i)
    !allocate (arr(i)%x, source = t2(i))
    print *, "is allocated"
    print *, allocated (arr(i)%x)
    x1 = arr(i)%x
    !deallocate(x1)
    !allocate(x1, source = arr(i)%x)
    !print *, "storage size x1"
    !print *, storage_size(x1)
    p=>x1
    print *, same_type_as(arr(i)%x, x1)
    select type (p)
      type is (t2)
        print *, p%j
      type is (t)
        print *, "type of p is t"
    end select
  end do

  contains
  function retPoly(p)
    class(*), pointer :: p
    class(t), allocatable :: retPoly
    select type (p)
      type is (t2)
        print *, p%j
        retPoly = t(null)
      class default
        retPoly = t(null)
    end select
  end function
end
