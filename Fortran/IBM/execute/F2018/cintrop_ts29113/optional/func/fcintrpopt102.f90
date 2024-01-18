! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June 25, 2012
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from Fortran, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*   the actual argument is a disassociated POINTER or an unallocated ALLOCATABLE
!*
!* Dummy Argument:
!*   dummy argument is not allocatable or pointer when -qxlf2008=checkpresence
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module testmod
   use iso_c_binding

   type, bind(c) :: dt0
      complex(c_float_complex) :: a
      logical(c_bool) :: b
      character(c_char) :: c
   end type

   contains

   subroutine test1(arg1, arg2) bind(c)
      integer(c_long), optional, intent(in) :: arg1
      integer(c_long), optional :: arg2

      if (present(arg1)) then
	   error stop 10
      endif

      if (present(arg2)) then
           error stop 10
      endif

   end subroutine test1

   subroutine test2(arg1, arg2) bind(c)
      real(c_float), optional, intent(in) :: arg1
      real(c_float), optional :: arg2

      if (present(arg1)) then
           error stop 30
      endif

      if (present(arg2)) then
           error stop 40
      endif

   end subroutine test2

   subroutine test3(arg1, arg2) bind(c)
      type(dt0), optional, intent(in) :: arg1
      type(dt0), optional :: arg2

      if (present(arg1)) then
           error stop 50
      endif

      if (present(arg2)) then
           error stop 60
      endif

   end subroutine test3

end module testmod

program testprogram
  use iso_c_binding
  use testmod
  implicit none

  integer(c_long), pointer :: pi_a
  integer(c_long), allocatable :: ai_a

  real(c_float), pointer :: pr_a
  real(c_float), allocatable :: ar_a

  type(dt0), pointer :: pt_a
  type(dt0), allocatable :: at_a

  nullify (pi_a)
  nullify (pr_a)
  nullify (pt_a)

  call test1()
  call test1(pi_a)
  call test1(ai_a)
  call test1(arg2=pi_a)
  call test1(arg2=ai_a)
  call test1(pi_a, ai_a)
  call test1(ai_a, pi_a)

  call test2()
  call test2(pr_a)
  call test2(ar_a)
  call test2(arg2=pr_a)
  call test2(arg2=ar_a)
  call test2(pr_a, ar_a)
  call test2(ar_a, pr_a)

  call test3()
  call test3(pt_a)
  call test3(at_a)
  call test3(arg2=pt_a)
  call test3(arg2=at_a)
  call test3(pt_a, at_a)
  call test3(at_a, pt_a)

end program
