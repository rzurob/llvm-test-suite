!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : IEEESelectedRealKind06f.f
!*  TEST CASE TITLE            :
!*
!*
!*  PROGRAMMER                 : Jin Li
!*  DATE                       : 10/20/2010
!*  ORIGIN                     : XL Fortran Compiler Development, IBM Torolab
!*
!*  PRIMARY FUNCTIONS TESTED   : return value of -5 from IEEE_selected_real_kind([P, R,
!*                               RADIX]) 
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the if the return
!*                               value -5 from IEEE_selected_real_kind is correctly
!*                               returned.
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program IEEESelectedRealKind06f
                use, intrinsic :: ieee_arithmetic
                integer l, m, n
                integer, parameter :: CONST_RETURN_MINUS_5 = -5

                interface
                integer function foo(N)
                        integer N
                end function
                end interface


                !**************************************************************!
                !************ P=15, R=330, and RADIX=1 ************************!
                !**************************************************************!
                
                !-- parameters as constants
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(15, 330, 1)) error stop 10_4

                !-- parameters as variables
                m = 15
                n = 330
                l = 1
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(m, n, l)) error stop 20_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 30_4
                
                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 40_4


                !**************************************************************!
                !************* P=6, R=308, and RADIX=4 ************************!
                !**************************************************************!
                
                !-- parameters as constants
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(6, 308, 4)) error stop 50_4

                !-- parameters as variables
                m = 6
                n = 308
                l = 4
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(m, n, l)) error stop 60_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 70_4
                
                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 80_4
                

                !**************************************************************!
                !************** P=0, R=308, and RADIX=8 ***********************!
                !**************************************************************!
                
                !-- parameters as constants
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(0, 308, 8)) error stop 90_4

                !-- parameters as variables
                m = 0
                n = 308
                l = 8
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(m, n, l)) error stop 100_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 110_4
                
                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 120_4
                

                !**************************************************************!
                !************** P=0, R=218, and RADIX=1 ***********************!
                !**************************************************************!
                
                !-- parameters as constants
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(0, 218, 1)) error stop 130_4

                !-- parameters as variables
                m = 0
                n = 218
                l = 1
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(m, n, l)) error stop 140_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 150_4
                
                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 160_4

                !**************************************************************!
                !************** P=5, R=164, and RADIX=-1 **********************!
                !**************************************************************!
                
                !-- parameters as constants
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(5, 164, -1)) error stop 170_4

                !-- parameters as variables
                m = 5
                n = 164
                l = -1
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(m, n, l)) error stop 180_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(foo(m), foo(n), foo(l))) error stop 190_4
                
                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_5 .ne. IEEE_selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 200_4
                
        end program


        integer function foo(N)
                integer N
                foo = N
        end function

