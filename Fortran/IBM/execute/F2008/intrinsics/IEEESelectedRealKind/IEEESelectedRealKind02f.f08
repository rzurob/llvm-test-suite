!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2010
!*
!*  PRIMARY FUNCTIONS TESTED   : return value of -1 from IEEE_selected_real_kind([P, R,
!*                               RADIX])
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the if the return
!*                               value -1 from IEEE_selected_real_kind is correctly
!*                               returned.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program IEEESelectedRealKind02f
                use, intrinsic :: ieee_arithmetic
                integer l, m, n
                integer, parameter :: CONST_RETURN_MINUS_1 = -1

                interface
                integer function foo(N)
                        integer N
                end function
                end interface


                !**************************************************************!
                !************ P=33, R=290, and RADIX=2 ************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(33, 290, 2)) error stop 10_4

                !-- parameters as variables
                m = 33
                n = 290
                l = 2
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(m, n, l)) error stop 20_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 30_4

                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 40_4


                !**************************************************************!
                !************** P=32, R=0, and RADIX=2 ************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(32, 0, 2)) error stop 50_4

                !-- parameters as variables
                m = 32
                n = 0
                l = 2
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(m, n, l)) error stop 60_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 70_4

                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 80_4


                !**************************************************************!
                !************** P=32, R=307, and RADIX=2 ************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(32, 307, 2)) error stop 90_4

                !-- parameters as variables
                m = 32
                n = 307
                l = 2
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(m, n, l)) error stop 100_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 110_4

                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_1 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 120_4


        end program


        integer function foo(N)
                integer N
                foo = N
        end function

