      subroutine check_c_to_f(c_arg2,c_len,n, extent, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg2(n+1:*)
        integer(C_INT) c_len,n, extent, test_no
        character(c_len) c_test
        if(c_len .NE. LEN(c_arg2)) then
           error STOP 1
        endif
        if(RANK(c_arg2) .NE. 1) then
           error STOP 2
        endif
        if(LBOUND(c_arg2,1) .NE. n+1) then
           print *, LBOUND(c_arg2,1)
           error STOP 3
        endif
        i = n+1
        if(test_no .EQ. 1) then
           c_test = 'C2F___'
        endif
        if(test_no .EQ. 2) then
           c_test = 'C2F2F'
        endif
        if(test_no .EQ. 3) then
           c_test = 'C2F2C'
        endif
        DO WHILE (i .LE. (extent+n))
          if(c_arg2(i) .NE. c_test) then
           error STOP 4
          endif
          i = i+1
        END DO

      end subroutine

      subroutine check_c_to_f_to_f(c_arg2,c_len,n,extent, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
         subroutine check_c_to_f(c_arg1, c_len,n,extent, test_no) bind(c)
           use, intrinsic :: iso_c_binding
           character(*) :: c_arg1(n+1:*)
           integer(C_INT) c_len,n,extent, test_no
         end subroutine
        end interface
        character(*) :: c_arg2(n+1:*)
        integer(C_INT) c_len,n, extent, test_no
        character(c_len) c_test
        call check_c_to_f(c_arg2, c_len,n,extent, test_no)
      end subroutine

      subroutine check_c_to_f_to_c(c_arg2,c_len,n, extent, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
         subroutine check_f_to_c(c_arg1, c_len,n, extent, test_no) bind(c)
           use, intrinsic :: iso_c_binding
           character(*) :: c_arg1(n+1:*)
           integer(C_INT) c_len,n, extent, test_no
         end subroutine
        end interface
        character(*) :: c_arg2(n+1:*)
        integer(C_INT) c_len,n, extent, test_no
        character(c_len) c_test
        call check_f_to_c(c_arg2,c_len,n,extent, test_no)
      end subroutine
