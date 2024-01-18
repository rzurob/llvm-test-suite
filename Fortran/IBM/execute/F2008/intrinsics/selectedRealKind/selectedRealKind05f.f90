!***********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectedRealKind05f.f
!*
!*  DATE                       : 10/20/2010
!*
!*  PRIMARY FUNCTIONS TESTED   : return value of -4 from SELECTED_REAL_KIND([P, R,
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
!*                               value -4 from selected_real_kind is correctly
!*                               returned.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program selectedRealKind05f
                integer l, m, n
                integer, parameter :: CONST_RETURN_MINUS_4 = -4

                interface
                integer function foo(N)
                        integer N
                end function
                end interface


                !**************************************************************!
                !************ P=17, R=300, and RADIX=2 ************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(17, 300, 2)) error stop 10_4

                !-- parameters as variables
                m = 17
                n = 300
                l = 2
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(m, n, l)) error stop 20_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(foo(m), foo(n), foo(l))) error stop 30_4

                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 40_4


                !**************************************************************!
                !************ P=31, R=307, and RADIX=2 ************************!
                !**************************************************************!

                !-- parameters as constants
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(31, 307, 2)) error stop 50_4

                !-- parameters as variables
                m = 31
                n = 307
                l = 2
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(m, n, l)) error stop 60_4

                !-- parameters as return value from dummy functions
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(foo(m), foo(n), foo(l))) error stop 70_4

                !-- explicitly assign parameter values
                if (CONST_RETURN_MINUS_4 .ne. selected_real_kind(R = foo(n), P = foo(m), RADIX = foo(l))) error stop 80_4


        end program


        integer function foo(N)
                integer N
                foo = N
        end function

