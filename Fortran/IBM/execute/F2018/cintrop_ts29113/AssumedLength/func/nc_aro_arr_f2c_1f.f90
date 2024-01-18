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

          subroutine check_f_to_c(c_arg1, i_len, a_rank, test_no) bind(c)
            USE, INTRINSIC :: ISO_C_BINDING
            character(*) :: c_arg1(..)
            integer(C_INT) i_len, a_rank, test_no
          end subroutine
          subroutine check_f_to_f(c_arg1, i_len, a_rank, test_no) bind(c)
            USE, INTRINSIC :: ISO_C_BINDING
            character(*) :: c_arg1(..)
            integer(C_INT) i_len, a_rank, test_no
          end subroutine
          end interface

          ! Explicit Shape array

          character(3), dimension(1) :: v_arg1
          character(4), dimension(1,1) :: v_arg2
          character(5), dimension(10) :: v_arg3
          character(6), dimension(5,5) :: v_arg4
          character(7), dimension(1:5, -5:1, 10) :: v_arg5
          character(8), STATIC :: v_arg6(10,10)
          character(9), VOLATILE :: v_arg7(5,5,5)
          character(3), AUTOMATIC :: v_arg8(3,3)
          character(6), TARGET  :: v_arg9(2,2)
          character(6), PARAMETER :: v_arg1_2(5,5) = "ABC123"
          character(7), TARGET :: v_arg1_3(10)


          ! Deferred Shape array
          character(6), pointer :: v_arg11(:,:)
          character(6), pointer, CONTIGUOUS :: v_arg12(:,:)
          character(:), pointer :: v_arg13(:)
          character(7), ALLOCATABLE, DIMENSION(:, :, :) :: v_arg14
          character(7), ALLOCATABLE :: v_arg15(:)
          character(:), ALLOCATABLE :: v_arg16(:,:)


          ! Implied Shape Array
          CHARACTER(5), PARAMETER :: v_arg21(4:*) = ["ABC", "DEF"]
          CHARACTER(*), PARAMETER :: v_arg22(4:*) = ["ABCDEF", "DEFGHI"]

          ! Explicit Shape array

          v_arg1 = "ABC"
          call check_f_to_c(v_arg1, LEN(v_arg1), rank(v_arg1), 1)
          call check_f_to_f(v_arg1, LEN(v_arg1), rank(v_arg1), 1)

          v_arg2 = "ABCD"
          call check_f_to_c(v_arg2, LEN(v_arg2), rank(v_arg2), 2)


          v_arg3 = "FIVES"
          call check_f_to_c(v_arg3(1:3), LEN(v_arg3),rank(v_arg3),3)

          v_arg4 = "SIXES1"
          call check_f_to_c(v_arg4, LEN(v_arg4), rank(v_arg4), 4)

          v_arg5 = "SEVENS"
          call check_f_to_c(v_arg5, LEN(v_arg5), rank(v_arg5), 5)

          v_arg6 = "EIGHTS"
          call check_f_to_c(v_arg6,LEN(v_arg6), rank(v_arg6), 6)

          v_arg7 = "NINES"
          call check_f_to_c(v_arg7, LEN(v_arg7), rank(v_arg7), 7)

          v_arg8 = "TEN"
          call check_f_to_c(v_arg8, LEN(v_arg8), rank(v_arg8), 8)

          v_arg9 = "TEST12"

          call check_f_to_c(v_arg9, LEN(v_arg9), rank(v_arg9), 9)

          call check_f_to_c(v_arg1_2,LEN(v_arg1_2), rank(v_arg1_2), 10)

          v_arg1_3 = "TEST13"

          ! Deferred Shape array

          v_arg11 => v_arg9
          v_arg12 => v_arg9
          v_arg13 => v_arg1_3

          call check_f_to_c(v_arg11,LEN(v_arg11), rank(v_arg11), 11)
          call check_f_to_c(v_arg12,LEN(v_arg12), rank(v_arg12), 12)
          call check_f_to_c(v_arg13,LEN(v_arg13), rank(v_arg13), 13)

          allocate(v_arg14(8, -1:3, 5))
          v_arg14 = "TEST14"
          call check_f_to_c(v_arg14, LEN(v_arg14), rank(v_arg14), 14)

          allocate(v_arg15(10))
          v_arg15 = "TEST15"
          call check_f_to_c(v_arg15, LEN(v_arg15), rank(v_arg15), 15)

          allocate(character(len=16) :: v_arg16(10,10))
          v_arg16 = "TEST"
          call check_f_to_c(v_arg16, LEN(v_arg16), rank(v_arg16), 16)  !defect 103037



           ! Implied Shape Array

          call check_f_to_c(v_arg21,LEN(v_arg21), rank(v_arg21), 21)
          call check_f_to_c(v_arg22,LEN(v_arg22), rank(v_arg22), 22)

          ! Explicit shape array:automatic

          call SUB1_1(10, 23)
          call SUB1_2(7, 24)

          ! adjustable
          call SUB2_1(1,v_arg3, 25)
          call SUB2_2(1,v_arg4, 26)

          ! assumed shape

          call SUB3_1(v_arg14, 27)
          call SUB3_2(v_arg14, 28)
          call SUB3_3(v_arg14, 29)


          ! assumed size

          call SUB4_1(v_arg1,3, 30)
          call SUB4_1(v_arg2,4, 31)
          call SUB4_1(v_arg3,5, 32)
          call SUB4_1(v_arg4,6, 33)

          call SUB4_2(v_arg1, 34)
          call SUB4_2(v_arg2, 35)
          call SUB4_2(v_arg3, 36)
          call SUB4_2(v_arg4, 37)

          call SUB4_3(v_arg5, 38)
          call SUB4_4(v_arg6, 39)
          call SUB4_5(v_arg7, 9, 40)

          ! assumed rank

          call SUB5_1(v_arg1,3, 41)
          call SUB5_1(v_arg2,4, 42)
          call SUB5_1(v_arg5,7, 43 )
          call SUB5_1(v_arg11, 6, 44)
          call SUB5_1(v_arg1_2,6, 45)

          call SUB5_2(v_arg11, 6, 46)
          call SUB5_3(v_arg5, 7, 47)
          call SUB5_4(v_arg1, 48)
          call SUB5_5(v_arg1_2, 49)
          call SUB5_6(v_arg1, 50)

          ! F2F

          call check_f_to_f(v_arg1, LEN(v_arg1), rank(v_arg1), 1)
          call check_f_to_f(v_arg2, LEN(v_arg2), rank(v_arg2), 2)
          call check_f_to_f(v_arg3(1:3), LEN(v_arg3),rank(v_arg3),3)
          call check_f_to_f(v_arg4, LEN(v_arg4), rank(v_arg4), 4)
          call check_f_to_f(v_arg5, LEN(v_arg5), rank(v_arg5), 5)
          call check_f_to_f(v_arg6,LEN(v_arg6), rank(v_arg6), 6)
          call check_f_to_f(v_arg7, LEN(v_arg7), rank(v_arg7), 7)
          call check_f_to_f(v_arg8, LEN(v_arg8), rank(v_arg8), 8)
          call check_f_to_f(v_arg9, LEN(v_arg9), rank(v_arg9), 9)
          call check_f_to_f(v_arg1_2,LEN(v_arg1_2), rank(v_arg1_2), 10)
          call check_f_to_f(v_arg11,LEN(v_arg11), rank(v_arg11), 11)
          call check_f_to_f(v_arg12,LEN(v_arg12), rank(v_arg12), 12)
          call check_f_to_f(v_arg13,LEN(v_arg13), rank(v_arg13), 13)
          call check_f_to_f(v_arg14, LEN(v_arg14), rank(v_arg14), 14)
          call check_f_to_f(v_arg15, LEN(v_arg15), rank(v_arg15), 15)
          call check_f_to_f(v_arg16, LEN(v_arg16), rank(v_arg16), 16)  !defect 103037
          call check_f_to_f(v_arg21,LEN(v_arg21), rank(v_arg21), 21)
          call check_f_to_f(v_arg22,LEN(v_arg22), rank(v_arg22), 22)

       contains
       ! Explicit shape array:
         ! automatic

       SUBROUTINE SUB1_1(Y, test_no)
         INTEGER Y, test_no
         CHARACTER(10) Z (20, 1:Y)
         Z  = "SUB1_1TEST"
         call check_f_to_c(Z, LEN(Z), rank(Z), test_no)
         call check_f_to_f(Z, LEN(Z), rank(Z), test_no)
       END SUBROUTINE

       SUBROUTINE SUB1_2(Y, test_no)
         INTEGER Y, test_no
         CHARACTER(Y) Z (5, 1:Y)
         Z  = "SUB1_2TEST"
         call check_f_to_c(Z, LEN(Z), rank(Z), test_no)
           call check_f_to_f(Z, LEN(Z), rank(Z), test_no)
       END SUBROUTINE

         ! adjustable
       SUBROUTINE SUB2_1(X, Y, test_no)
         INTEGER X, test_no
         CHARACTER(5) Y(X*3)
         call check_f_to_c(Y,LEN(Y) , rank(Y), test_no)
         call check_f_to_f(Y,LEN(Y) , rank(Y), test_no)
       END SUBROUTINE

       SUBROUTINE SUB2_2(X, Y, test_no)
         INTEGER X, test_no
         CHARACTER(X) Y(X*1)
         call check_f_to_c(Y, LEN(Y), rank(Y), test_no)
         call check_f_to_f(Y,LEN(Y) , rank(Y), test_no)
       END SUBROUTINE

         ! assumed shape
       SUBROUTINE SUB3_1(B, test_no)
         CHARACTER(7) B(1:,:,10:)
         integer test_no
         call check_f_to_c(B, 7, rank(B), test_no)
         call check_f_to_f(B, 7, rank(B), test_no)
       END SUBROUTINE

       SUBROUTINE SUB3_2(B, test_no)
         CHARACTER(*) B(1:,:,10:)
         integer test_no
           call check_f_to_c(B, 7, rank(B), test_no)
         call check_f_to_f(B, 7, rank(B), test_no)
       END SUBROUTINE


       SUBROUTINE SUB3_3(B, test_no)
         CHARACTER(*), CONTIGUOUS ::  B(1:,:,10:)
         integer test_no
         call check_f_to_c(B, 7, rank(B), test_no)
         call check_f_to_f(B, 7, rank(B), test_no)
       END SUBROUTINE

         ! assumed size
       SUBROUTINE SUB4_1(ARRAY, L, test_no)
         CHARACTER(*) ARRAY(*)
         integer L, test_no
         call check_f_to_c(ARRAY, L, rank(ARRAY), test_no)
         call check_f_to_f(ARRAY, L, rank(ARRAY), test_no)
       END SUBROUTINE

       SUBROUTINE SUB4_2(ARRAY, test_no)
         CHARACTER(5) ARRAY(*)
         integer test_no
         call check_f_to_c(ARRAY, 5, rank(ARRAY), test_no)
         call check_f_to_f(ARRAY, 5, rank(ARRAY), test_no)
       END SUBROUTINE


       SUBROUTINE SUB4_3(ARRAY, test_no)
         CHARACTER(10) ARRAY(2,*)
         integer test_no
         call check_f_to_c(ARRAY, 10, rank(ARRAY), test_no)
         call check_f_to_f(ARRAY, 10, rank(ARRAY), test_no)
       END SUBROUTINE

       SUBROUTINE SUB4_4(ARRAY, test_no)
         CHARACTER(10) ARRAY(2,-4:*)
         integer test_no
         call check_f_to_c(ARRAY, 10, rank(ARRAY), test_no)
         call check_f_to_f(ARRAY, 10, rank(ARRAY), test_no)
       END SUBROUTINE

       SUBROUTINE SUB4_5(ARRAY, L, test_no)
         CHARACTER(*) ARRAY(2,-4:*)
         integer L, test_no

         call check_f_to_c(ARRAY, L, rank(ARRAY), test_no)
         call check_f_to_f(ARRAY, L, rank(ARRAY), test_no)
       END SUBROUTINE

         ! assumed rank
       SUBROUTINE SUB5_1(a, L, test_no)
          CHARACTER(*) :: a(..)
          integer L, test_no
          call check_f_to_c(a, LEN(a), rank(a), test_no)
          call check_f_to_f(a, LEN(a), rank(a), test_no)
       END

       SUBROUTINE SUB5_2(a, L, test_no)
         CHARACTER(*), POINTER :: a(..)
          integer L, test_no
          call check_f_to_c(a, LEN(a), rank(a), test_no)
          call check_f_to_f(a, LEN(a), rank(a), test_no)
       END

       SUBROUTINE SUB5_3(a,L, test_no)
          CHARACTER(*), CONTIGUOUS :: a(..)
          integer L, test_no
          call check_f_to_c(a, LEN(a), rank(a), test_no)
           call check_f_to_f(a, LEN(a), rank(a), test_no)
       END

       SUBROUTINE SUB5_4(a, test_no)
          CHARACTER(*) :: a(..)
          integer test_no
          call check_f_to_c(a, LEN(a), rank(a), test_no)
          call check_f_to_f(a, LEN(a), rank(a), test_no)
       END

       SUBROUTINE SUB5_5(a, test_no)
          CHARACTER(*) :: a(..)
          integer test_no
          call check_f_to_c(a, LEN(a), rank(a), test_no)
          call check_f_to_f(a, LEN(a), rank(a), test_no)
       END

       SUBROUTINE SUB5_6(a, test_no)
          CHARACTER(*), CONTIGUOUS :: a(..)
          integer test_no
          call check_f_to_c(a, LEN(a), rank(a), test_no)
          call check_f_to_f(a, LEN(a), rank(a), test_no)
       END

       end program


        subroutine check_f_to_f(c_arg1, i_len, a_rank, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg1(..)
        integer(C_INT) i_len, a_rank, test_no
        if(i_len .NE. LEN(c_arg1)) then
           error STOP 1
        endif
        if(RANK(c_arg1) .NE. a_rank) then
           print *, RANK(c_arg1)
           error STOP 2
        endif
       end subroutine








