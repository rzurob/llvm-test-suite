!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 13, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 916820
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Fortran 2008 allows procedure calls where:
!*	  1. The dummy argument has the POINTER and INTENT(IN) attributes, and
!*	  2. The actual argument is a nonpointer that has the TARGET attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890

interface
  subroutine sub(p1, p2)
    integer, pointer, intent(in) :: p1, p2
  end subroutine
end interface

interface
  subroutine logproc(l1, l2)
    logical, pointer, intent(in) :: l1, l2
  end subroutine
end interface

integer, target :: t1
integer, target :: t2

real, target :: r1
real, target :: r2
real :: r3

complex, target :: c1, c2

logical :: l1, l2

t1 = 5
t2 = 7
call sub(t1, t2) ! Hello World 12

t1 = 100
t2 = -200
call sub(t1, t2) ! Hello World -100

r1 = 3.0
r2 = 5.0
r3 = r1 + r2 - 0.1

print "(f4.1)", minfloat(r1, r2) ! -2.0

r1 = r3 - r1
r2 = 3.4

print "(f4.1)", minfloat(r1, r2) ! 1.5

r1 = 3.0
r2 = 4.0
c1 = (r1, r2)
c2 = (r2, r1)

call cmp(c1, c2) ! 24.0

c2 = (r1, r2)
call cmp(c1, c2) ! 25.0

l1 = .true.
l2 = .false.
call log(l1, l2)

l1 = .false.
l2 = .false.
call log(l1, l2)

contains
  real function minfloat(r1, r2)
    real, pointer, intent(in) :: r1, r2

    minfloat = r1 - r2
  end

  subroutine cmp(c1, c2)
    complex, pointer, intent(in) :: c1, c2

        print "(f4.1)", c1%RE * c2%RE + c1%IM * c2%IM
  end

  subroutine log(l1, l2)
    logical, target :: l1, l2

    call logproc(l1, l2)
  end subroutine
end

subroutine sub(p1, p2)
  integer, pointer, intent(in) :: p1, p2

  print *, "Hello World"
  print *, p1 + p2

  p1 = -1
  p2 = -2
end subroutine

subroutine logproc(l1, l2)
  logical, pointer, intent(in) :: l1, l2

  print *, l1 .and. l2
  print *, l2 .or. l1
end subroutine
