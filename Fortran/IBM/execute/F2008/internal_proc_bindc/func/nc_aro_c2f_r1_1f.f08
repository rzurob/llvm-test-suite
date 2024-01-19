program fortranMain
  use iso_c_binding

  interface
    subroutine cfun(x) bind(c)
      use iso_c_binding, only : c_funptr
      type(c_funptr), value :: x
    end subroutine cfun
  end interface
  type(c_funptr)  :: check_c_to_f_ptr
  check_c_to_f_ptr = c_funloc(check_c_to_f)

  call cfun(check_c_to_f_ptr)

  contains
  subroutine check_c_to_f(c_arg2,c_len,test_no) bind(c)
    use, intrinsic :: iso_c_binding
    character(*) :: c_arg2(..)
    integer(C_INT) c_len, test_no
    character(c_len) c_test
    if(c_len .NE. LEN(c_arg2)) then
     error STOP 1
    endif
    if(RANK(c_arg2) .NE. 1) then
      error STOP 2
    endif
    if(LBOUND(c_arg2,1) .NE. 1) then
      error STOP 3
    endif
    if(SIZE(c_arg2,1) .NE. 5) then
       error STOP 4
    endif
  end subroutine
end program fortranMain

subroutine check_c_to_f_to_f(c_arg2,c_len,test_no,cfunptr) bind(c)
  use, intrinsic :: iso_c_binding
  interface
   subroutine check_c_to_f(c_arg1, c_len, test_no) bind(c)
     use, intrinsic :: iso_c_binding
     character(*) :: c_arg1(..)
     integer(C_INT) c_len, test_no
   end subroutine
  end interface
  type(c_funptr)  :: cfunptr
  procedure(check_c_to_f), pointer :: ffunptr
  character(*) :: c_arg2(..)
  integer(C_INT) c_len, test_no
  character(c_len) c_test
  call c_f_procpointer(cfunptr, ffunptr)
  call ffunptr(c_arg2, c_len, test_no)
end subroutine

subroutine check_c_to_f_to_c(c_arg2,c_len, test_no) bind(c)
  use, intrinsic :: iso_c_binding
  interface
   subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
     use, intrinsic :: iso_c_binding
     character(*) :: c_arg1(..)
     integer(C_INT) c_len,test_no
   end subroutine
  end interface
  character(*) :: c_arg2(..)
  integer(C_INT) c_len, test_no
  character(c_len) c_test
  call check_f_to_c(c_arg2,c_len, test_no)
end subroutine
