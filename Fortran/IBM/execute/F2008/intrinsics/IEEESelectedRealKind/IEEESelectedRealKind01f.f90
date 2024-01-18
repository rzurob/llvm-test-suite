!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2010
!*
!*  PRIMARY FUNCTIONS TESTED   : return value from IEEE_selected_real_kind([P, R,
!*                               RADIX]) is correctly calculated
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the if the return
!*                               value from IEEE_selected_real_kind is correctly
!*                               calculated in certain scenarios.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program IEEESelectedRealKind01f
                use, intrinsic :: ieee_arithmetic
                integer l, m, n
                BYTE    b
                real(4) p
                real(8) q
                real(16) r
                integer, parameter :: CONST_KIND_4 = 4
                integer, parameter :: CONST_KIND_8 = 8
                integer, parameter :: CONST_KIND_16 = 16

                interface
                integer function foo(N)
                        integer N
                end function
                end interface


                !**************************************************************!
                !****** Test KIND of 4 ***********************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(6, 37, 2)) error stop 10_4
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(6, 37, 2_1)) error stop 11_4
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(6, 37, 2_2)) error stop 12_4
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(6, 37, 2_4)) error stop 13_4
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(6, 37, 2_8)) error stop 14_4

                !-- parameters as variables
                m = 6
                n = 37
                l = 2
                b = 2
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(m, n, l)) error stop 20_4
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(m, n, b)) error stop 21_4


                !-- parameters as return value from dummy functions
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 30_4

                !-- explicitly assign parameter values
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 40_4

                !-- use the return value from built-in functions (RANGE(), PRECISION(), RADIX())
                if (CONST_KIND_4 .ne. IEEE_selected_real_kind(R = RANGE(p), P = PRECISION(p), RADIX = RADIX(p))) error stop 41_4



                !**************************************************************!
                !****** Test KIND of 8 ***********************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_KIND_8 .ne. IEEE_selected_real_kind(15, 307, 2)) error stop 50_4

                !-- parameters as variables
                m = 15
                n = 307
                l = 2
                if (CONST_KIND_8 .ne. IEEE_selected_real_kind(m, n, l)) error stop 60_4

                !-- parameters as return value from dummy functions
                if (CONST_KIND_8 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 70_4

                !-- explicitly assign parameter values
                if (CONST_KIND_8 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 80_4

                !-- use the return value from built-in functions (RANGE(), PRECISION(), RADIX())
                if (CONST_KIND_8 .ne. IEEE_selected_real_kind(R = RANGE(q), P = PRECISION(q), RADIX = RADIX(q))) error stop 81_4

                !**************************************************************!
                !****** Test KIND of 16 ***********************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_KIND_16 .ne. IEEE_selected_real_kind(31, 291, 2)) error stop 90_4

                !-- parameters as variables
                m = 31
                n = 291
                l = 2
                if (CONST_KIND_16 .ne. IEEE_selected_real_kind(m, n, l)) error stop 100_4

                !-- parameters as return value from dummy functions
                if (CONST_KIND_16 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 110_4

                !-- explicitly assign parameter values
                if (CONST_KIND_16 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 120_4

                !-- use the return value from built-in functions (RANGE(), PRECISION(), RADIX())
                if (CONST_KIND_16 .ne. IEEE_selected_real_kind(R = RANGE(r), P = PRECISION(r), RADIX = RADIX(r))) error stop 121_4

        end program


        integer function foo(N)
                integer N
                foo = N
        end function

