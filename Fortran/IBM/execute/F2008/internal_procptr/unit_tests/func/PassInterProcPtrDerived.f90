!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 24, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 303977
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Now Fortran 2008 allows internal procedures and pointers to such procedures
!*  to be actual arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type, public :: base
    integer :: i = 3
    real :: j = 4.0
  end type

  type, extends(base) :: child1(k)
    integer, kind :: k
    integer(k) :: value
  end type

  type, extends(base) :: child2(l)
    integer, len :: l
    character(l) :: c
  end type

  contains
    subroutine test_pass_inter_proc1(test_dt)
      interface
        subroutine test_dt(dt)
          import base
          type(base) :: dt
        end subroutine
      end interface

      type(base) :: dt
      dt % j = 7.3

      call test_dt(dt)
    end

    subroutine test_pass_inter_proc2(test_dt)
      interface
        subroutine test_dt(dt)
          import base
          class(base) :: dt
        end subroutine
      end interface

      class(base), allocatable :: dt
      type(base) :: b = base(5, 1.7)
      type(child1(4)) :: c1
      type(child2(5)) :: c2

      c1%value = 777
      c2%c = "hello"

      allocate(dt, source=b)
      call test_dt(dt)

      deallocate(dt)
      allocate(dt, source=c1)
      call test_dt(dt)

      deallocate(dt)
      allocate(dt, source=c2)
      call test_dt(dt)
    end

    subroutine test_pass_inter_proc3(test_dt)
      interface
        subroutine test_dt(dt)
          import base
          class(*) :: dt
        end subroutine
      end interface

      class(*), allocatable :: dt
      integer :: i = 987
      type(base) :: b = base(5, 1.7)
      type(child1(4)) :: c1
      type(child2(5)) :: c2

      c1%value = 777
      c2%c = "hello"

      allocate(dt, source=i)
      call test_dt(dt)

      deallocate(dt)
      allocate(dt, source=b)
      call test_dt(dt)

      deallocate(dt)
      allocate(dt, source=c1)
      call test_dt(dt)

      deallocate(dt)
      allocate(dt, source=c2)
      call test_dt(dt)
    end
end module

program PassInterProcPtrDerived
  use m

  interface
    subroutine sub1(dt)
      import base
      type(base) :: dt
    end subroutine
  end interface

  interface
    subroutine sub2(dt)
      import base
      class(base) :: dt
    end subroutine
  end interface

  interface
    subroutine sub3(dt)
      import base
      class(*) :: dt
    end subroutine
  end interface

  procedure(sub1), pointer :: p1
  procedure(sub2), pointer :: p2
  procedure(sub3), pointer :: p3

  p1 => test_dt1
  p2 => test_dt2
  p3 => test_dt3

  call test_pass_inter_proc1(p1)
  call test_pass_inter_proc2(p2)
  call test_pass_inter_proc3(p3)

  contains
    subroutine test_dt1(dt) ! derived-type
      type(base) :: dt

      print *, dt % i
      print "(f3.1)", dt % j
    end

    subroutine test_dt2(dt) ! polymorphic
      class(base) :: dt

      select type(dt)
        type is (base)
          print "(a, i4, f3.1)", " base is:", dt%i, dt%j
        type is (child1(4))
          print *, "child1-value:", dt%value
        type is (child2(5))
          print *, "child2-string:", dt%c
      end select
    end

    subroutine test_dt3(dt) ! unlimited-polymorphic
      class(*) :: dt

      select type(dt)
        type is (integer)
          print *, "integer is:", dt
        type is (base)
          print "(a, i4, f3.1)", " base is:", dt%i, dt%j
        type is (child1(4))
          print *, "child1-value:", dt%value
        type is (child2(5))
          print *, "child2-string:", dt%c
      end select
    end
end
