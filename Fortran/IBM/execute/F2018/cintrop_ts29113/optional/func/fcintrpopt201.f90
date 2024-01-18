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
!* Calling a BIND(C) procedure from Fortran, (where the procedure is defined in C)
!*
!* Actual Argument:
!*  the actual argument is a disassociated POINTER or an unallocated ALLOCATABLE
!*
!* Dummy Argument:
!*  dummy argument is not pointer when -qxlf2008=checkpresence
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
  interface
   subroutine c_sub_test1(arg1, arg2) bind(c)
     use iso_c_binding
     integer(c_int), optional :: arg1
     real(c_float), optional :: arg2
   end subroutine c_sub_test1

   integer(c_size_t) function c_func_test2(arg1) bind(c)
     use iso_c_binding
     integer(c_int64_t), optional :: arg1
   end function c_func_test2
  end interface
end module

program testprogram
  use iso_c_binding
  use testmod
  implicit none

  integer(c_int), pointer :: pi_a
  integer(c_int), allocatable :: ai_a

  real(c_float), pointer :: pr_a
  real(c_float), allocatable :: ar_a

  integer(c_int64_t), pointer :: pi64_a
  integer(c_int64_t), allocatable :: ai64_a

  integer(c_size_t) :: size1

  nullify (pi_a)
  nullify (pr_a)
  nullify (pi64_a)

  call c_sub_test1(pi_a)
  call c_sub_test1(ai_a)
  call c_sub_test1(arg2=pr_a)
  call c_sub_test1(arg2=ar_a)
  call c_sub_test1(pi_a, ar_a)
  call c_sub_test1(ai_a, pr_a)

  size1 = c_func_test2(pi64_a)

  size1 = c_func_test2(ai64_a)

end program
