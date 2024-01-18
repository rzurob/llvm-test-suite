program main
  implicit none
  interface gen7
    subroutine sub71(a)
      integer, allocatable :: a(..)
    end subroutine

    subroutine sub72(a)
      integer, pointer :: a(..)
    end subroutine
  end interface

  interface gen8
    subroutine sub81(a) bind(c)
      integer, allocatable :: a(..)
    end subroutine

    subroutine sub82(a) bind(c)
      integer, pointer :: a(..)
    end subroutine
  end interface

  integer, allocatable :: x(:)
  integer, pointer :: y(:, :)

  allocate(x(10), source=1)
  allocate(y(4, 4), source=2)

  call gen7(x)
  call gen7(y)
  call gen8(x)
  call gen8(y)
end program

subroutine sub71(a)
  integer, allocatable :: a(..)
  print *, 'sub71'
end subroutine

subroutine sub72(a)
  integer, pointer :: a(..)
  print *, 'sub72'
end subroutine

subroutine sub81(a) bind(c)
  integer, allocatable :: a(..)
  print *, 'sub81'
end subroutine

subroutine sub82(a) bind(c)
  integer, pointer :: a(..)
  print *, 'sub82'
end subroutine
