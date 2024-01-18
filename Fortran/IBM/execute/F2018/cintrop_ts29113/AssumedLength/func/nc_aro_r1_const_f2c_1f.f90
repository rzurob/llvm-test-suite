! *********************************************************************
!*  ===================================================================
!*
!*  DATE                : June, 1, 2014
!*  FEATURE             : RTC Master Story:
!*                        C Interop: Assumed-length Character arguments
!*                        (master story) (72333)
!*
!*  FEATURE             : C Interop: Assumed-length Character arguments
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      program assumed_lenght001

        interface
          subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(..)  :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(..) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(..) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(..) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_c_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(..) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine

        end interface

        ! F2C

        print *, "F2C"

        !a) Section of character literal
        call check_f_to_c("",LEN(""),1 )
        call check_f_to_c('',LEN(''),1)
        call check_f_to_c('ABC 123 "test"', LEN('ABC 123 "test"'), 2)
        call check_f_to_c("123 abc 'test'", LEN("123 abc 'test'"), 3)
        call check_f_to_c('Test ''apostrophes''',LEN('Test ''apostrophes'''), 4)
        !'
        call check_f_to_c("Test ""double quotation""", LEN("Test ""double quotation"""), 5)
        !"
        call check_f_to_c("Test newline\n", LEN("Test newline\n"), 6)
        call check_f_to_c(1_"a", LEN(1_"a"), 7)
        call check_f_to_c(2_"b", LEN(2_"b"), 8)
        call check_f_to_c('a\bcde\fg', LEN('a\bcde\fg'),9)

        !b) Section of character literal subobject

        call check_f_to_c('ABCDEFGHIJKL'(1:12), LEN('ABCDEFGHIJKL'(1:12)), 10)
        call check_f_to_c('EFGHIJKL'(1:4), LEN('EFGHIJKL'(1:4)), 11)

        !c) charcter expression

        call check_f_to_c('ABC'//'123', LEN('ABC'//'123'), 12)
        call check_f_to_c('ABC'//'123'(1:2), LEN('ABC'//'123'(1:2)),13)

        ! F2F
        print *, "F2F"

        !a) Section of character literal

        call check_f_to_f("",LEN(""),1 )
        call check_f_to_f('',LEN(''),1)
        call check_f_to_f('ABC 123 "test"', LEN('ABC 123 "test"'), 2)
        call check_f_to_f("123 abc 'test'", LEN("123 abc 'test'"), 3)
        call check_f_to_f('Test ''apostrophes''',LEN('Test ''apostrophes'''), 4)
        !'
        call check_f_to_f("Test ""double quotation""", LEN("Test ""double quotation"""), 5)
        !"
        call check_f_to_f("Test newline\n", LEN("Test newline\n"), 6)
        call check_f_to_f(1_"a", LEN(1_"a"), 7)
        call check_f_to_f(2_"b", LEN(2_"b"), 8)
        call check_f_to_f('a\bcde\fg', LEN('a\bcde\fg'),9)

        !b) Section of character literal subobject

        call check_f_to_f('ABCDEFGHIJKL'(1:12), LEN('ABCDEFGHIJKL'(1:12)), 10)
        call check_f_to_f('EFGHIJKL'(1:4), LEN('EFGHIJKL'(1:4)), 11)

        !c) charcter expression

        call check_f_to_f('ABC'//'123', LEN('ABC'//'123'), 12)
        call check_f_to_f('ABC'//'123'(1:2), LEN('ABC'//'123'(1:2)),13)

          ! F2C2F
        print *, "F2C2F"

        !a) Section of character literal
        call check_f_to_c_to_f("",LEN(""),1 )
        call check_f_to_c_to_f('',LEN(''),1)
        call check_f_to_c_to_f('ABC 123 "test"', LEN('ABC 123 "test"'), 2)
        call check_f_to_c_to_f("123 abc 'test'", LEN("123 abc 'test'"), 3)
        call check_f_to_c_to_f('Test ''apostrophes''',LEN('Test ''apostrophes'''), 4)
        !'
        call check_f_to_c_to_f("Test ""double quotation""", LEN("Test ""double quotation"""), 5)
        !"
        call check_f_to_c_to_f("Test newline\n", LEN("Test newline\n"), 6)
        call check_f_to_c_to_f(1_"a", LEN(1_"a"), 7)
        call check_f_to_c_to_f(2_"b", LEN(2_"b"), 8)
        call check_f_to_c_to_f('a\bcde\fg', LEN('a\bcde\fg'),9)

        !b) Section of character literal subobject

        call check_f_to_c_to_f('ABCDEFGHIJKL'(1:12), LEN('ABCDEFGHIJKL'(1:12)), 10)
        call check_f_to_c_to_f('EFGHIJKL'(1:4), LEN('EFGHIJKL'(1:4)), 11)

        !c) charcter expression

        call check_f_to_c_to_f('ABC'//'123', LEN('ABC'//'123'), 12)
        call check_f_to_c_to_f('ABC'//'123'(1:2), LEN('ABC'//'123'(1:2)),13)

          ! F2F2C
        print *, "F2F2C"

        !a) Section of character literal
        call check_f_to_f_to_c("",LEN(""),1 )
        call check_f_to_f_to_c('',LEN(''),1)
        call check_f_to_f_to_c('ABC 123 "test"', LEN('ABC 123 "test"'), 2)
        call check_f_to_f_to_c("123 abc 'test'", LEN("123 abc 'test'"), 3)
        call check_f_to_f_to_c('Test ''apostrophes''',LEN('Test ''apostrophes'''), 4)
        !'
        call check_f_to_f_to_c("Test ""double quotation""", LEN("Test ""double quotation"""), 5)
        !"
        call check_f_to_f_to_c("Test newline\n", LEN("Test newline\n"), 6)
        call check_f_to_f_to_c(1_"a", LEN(1_"a"), 7)
        call check_f_to_f_to_c(2_"b", LEN(2_"b"), 8)
        call check_f_to_f_to_c('a\bcde\fg', LEN('a\bcde\fg'),9)

        !b) Section of character literal subobject

        call check_f_to_f_to_c('ABCDEFGHIJKL'(1:12), LEN('ABCDEFGHIJKL'(1:12)), 10)
        call check_f_to_f_to_c('EFGHIJKL'(1:4), LEN('EFGHIJKL'(1:4)), 11)

        !c) charcter expression

        call check_f_to_f_to_c('ABC'//'123', LEN('ABC'//'123'), 12)
        call check_f_to_f_to_c('ABC'//'123'(1:2), LEN('ABC'//'123'(1:2)),13)


      end program

      subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), DIMENSION(..) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        if(c_len .NE. LEN(c_arg2)) then
           error STOP 1
        endif
        if(RANK(c_arg2) .NE. 0) then
           error STOP 2
        endif
       end subroutine

      subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(..) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
        end interface
        character(*), DIMENSION(..) :: c_arg3
        integer(C_INT) c_len, test_no
        call check_f_to_c(c_arg3,LEN(c_arg3), test_no)
       end subroutine
       subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(..) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine
        end interface
        character(*), DIMENSION(..) :: c_arg3
        integer(C_INT) c_len, test_no
        call check_f_to_f(c_arg3,LEN(c_arg3), test_no)
       end subroutine



