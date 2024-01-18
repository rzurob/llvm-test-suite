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

       subroutine esa3r_1(c_arg1, i_len, d0_ext, d1_ext, d2_ext) bind(c)
            USE, INTRINSIC :: ISO_C_BINDING
            character(*), DIMENSION(2, -1:1, 0:1) :: c_arg1
            integer(C_INT) i_len, d0_ext, d1_ext, d2_ext
          end subroutine
       end interface

          ! Explicit Shape array

          character(3), dimension(12) :: v_arg1
          character(4), dimension(-5:1,5) :: v_arg2
          character(5), dimension(13) :: v_arg3
          character(6), dimension(5,5) :: v_arg4
          character(7), dimension(1:5, -5:5, 10) :: v_arg5
          character(8), STATIC :: v_arg6(10,10)
          character(9), VOLATILE :: v_arg7(2, -1:1, 0:1)
          character(3), AUTOMATIC :: v_arg8(4,4)
          character(6), TARGET  :: v_arg9(5,5)
          character(6), PARAMETER :: v_arg1_2(5,5) = "ABC123"
          character(7), TARGET :: v_arg1_3(15)


          ! Deferred Shape array
          character(6), pointer :: v_arg11(:,:)
          character(6), pointer, CONTIGUOUS :: v_arg12(:,:)
          character(:), pointer :: v_arg13(:)
          character(7), ALLOCATABLE, DIMENSION(:, :, :) :: v_arg14
          character(7), ALLOCATABLE :: v_arg15(:)
          character(:), ALLOCATABLE :: v_arg16(:,:)



          ! Implied Shape Array
          CHARACTER(2), PARAMETER::v_arg21(4:*)=["AB","DE","EF","GH","IJ"]
          !"
          CHARACTER(*), PARAMETER :: v_arg22(4:*) = ["ABCDEF", "DEFGHI"]


          v_arg1 = "ABC"

          call esa3r_1(v_arg1, LEN(v_arg1), 2,3,2)

          v_arg2 = "ABCD"

          call esa3r_1(v_arg2, LEN(v_arg2),  2,3,2)

          v_arg3(1:2) = "FIVES"

          call esa3r_1(v_arg3(1:3), LEN(v_arg3),  2,3,2)

          v_arg4(1:5,1:5) = "SIXES1"

          call esa3r_1(v_arg4, 6,  2,3,2)
          v_arg5(1:5, -5:-5, 1:1) = "SEVENS"

          call esa3r_1(v_arg5, 7,  2,3,2)

          v_arg6 = "EIGHTS"

          call esa3r_1(v_arg6,8,  2,3,2)

          v_arg7 = "NINES"


          call esa3r_1(v_arg7, 9,  2,3,2)

          v_arg8 = "TEN"

          call esa3r_1(v_arg8, LEN(v_arg8),  2,3,2)

          v_arg9 = "TEST12"
          call esa3r_1(v_arg9, 6,  2,3,2)

          call esa3r_1(v_arg1_2,6,  2,3,2)

          v_arg1_3 = "TEST13"

          ! Deferred Shape array

          v_arg11 => v_arg9
          v_arg12 => v_arg9
          v_arg13 => v_arg1_3

          call esa3r_1(v_arg11,6,  2,3,2)  !defect 103260
          call esa3r_1(v_arg12,6,  2,3,2)
          call esa3r_1(v_arg13,7,  2,3,2)

          allocate(v_arg14(8, -1:3, 5))
          v_arg14 = "TEST14"
          call esa3r_1(v_arg14, LEN(v_arg14), 2,3,2)

          allocate(v_arg15(12))
          v_arg15 = "TEST15"
          call esa3r_1(v_arg15, LEN(v_arg15),2,3,2)

          allocate(character(len=16) :: v_arg16(10,10))
          v_arg16 = "TEST"
          call esa3r_1(v_arg16, LEN(v_arg16), 2,3,2)  !defect 103037



          ! Implied Shape Array
          call esa3r_1(v_arg21,LEN(v_arg21),2,3,2)
          call esa3r_1(v_arg22,LEN(v_arg22),2,3,2)

          ! Explicit shape array:automatic

          call SUB1_1(10)
          call SUB1_2(7)

          call SUB2_1(10, v_arg1)
          call SUB2_2(5, v_arg2)

          ! assumed shape

          call SUB3_1(v_arg14)
          call SUB3_2(v_arg14)
          call SUB3_3(v_arg14)


          ! assumed size

          !call SUB4_1(v_arg1,3) !defect 103255
          call SUB4_1(v_arg2,4, 2)
          call SUB4_1(v_arg3,5, 1)
          call SUB4_1(v_arg4,6, 2)

          call SUB4_2(v_arg1, 1)
          call SUB4_2(v_arg2, 2)
          call SUB4_2(v_arg3, 1)
          call SUB4_2(v_arg4, 2)

          call SUB4_3(v_arg5, LEN(v_arg5))
          call SUB4_4(v_arg6, LEN(v_arg6))
          call SUB4_5(v_arg7, LEN(v_arg7),  9)


      contains
       ! Explicit shape array:
         ! automatic

       SUBROUTINE SUB1_1(Y)
         INTEGER Y
         CHARACTER(10) Z (20, 1:Y)
         Z  = "SUB1_1TEST"

         call esa3r_1(Z, 10,  2,3,2)
       END SUBROUTINE

       SUBROUTINE SUB1_2(Y)
         INTEGER Y
         CHARACTER(Y) Z (5, 1:Y)
         Z  = "SUB1_2TEST"


         call esa3r_1(Z, 7,  2,3,2)
       END SUBROUTINE

         ! adjustable
       SUBROUTINE SUB2_1(X, Y)
         INTEGER X
         CHARACTER(10) Y(X*3)
         Y = "ADJUSTABLE"
         call esa3r_1(Y, 10,  2,3,2)
       END SUBROUTINE

       SUBROUTINE SUB2_2(X, Y)
         INTEGER X
         CHARACTER(*) Y(X*3)
         Y = "ADJUSTABLE"
         call esa3r_1(Y, LEN(Y),2,3,2)
       END SUBROUTINE

         ! assumed shape
       SUBROUTINE SUB3_1(B)
         CHARACTER(7) B(1:,:,10:)
         call esa3r_1(B, LEN(B),  2,3,2)
       END SUBROUTINE

       SUBROUTINE SUB3_2(B)
         CHARACTER(*) B(1:,:,10:)
         call esa3r_1(B, LEN(B),  2,3,2)
       END SUBROUTINE


       SUBROUTINE SUB3_3(B)
         CHARACTER(*), CONTIGUOUS ::  B(1:,:,10:)
         call esa3r_1(B, 7,  2,3,2)
       END SUBROUTINE

         ! assumed size
       SUBROUTINE SUB4_1(ARRAY, L, D)
         CHARACTER(*) ARRAY(*)
         integer L, D
         call esa3r_1(ARRAY, L,  2,3,2)
       END SUBROUTINE

       SUBROUTINE SUB4_2(ARRAY, D)
         CHARACTER(5) ARRAY(*) ! Extent value huge
         integer D
         call esa3r_1(ARRAY, 5,  2,3,2)
       END SUBROUTINE


       SUBROUTINE SUB4_3(ARRAY, D)
         CHARACTER(10) ARRAY(2,*)
         integer D
         call esa3r_1(ARRAY, 10,  2,3,2)
       END SUBROUTINE

       SUBROUTINE SUB4_4(ARRAY, D)
         CHARACTER(10) ARRAY(3,-4:*)
         integer D
         call esa3r_1(ARRAY, 10,  2,3,2)
       END SUBROUTINE

        SUBROUTINE SUB4_5(ARRAY,L, D)
         CHARACTER(*) ARRAY(5,5,-4:*)
         integer L, D
         call esa3r_1(ARRAY, L,  2,3,2)
       END SUBROUTINE
       end program

