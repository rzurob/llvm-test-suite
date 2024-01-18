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
            character(*), DIMENSION(5) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
          FUNCTION func1 () RESULT (v_char)
            character(11) v_char(10)
          END FUNCTION func1
          FUNCTION func2 (c_arg) RESULT (v_char)
            character(11) :: v_char(10)
            character(*), DIMENSION(1) :: c_arg
          END FUNCTION func2


        end interface

        ! a) Explicit Shape array

        ! --DIMENSION attribute

          character(4), dimension(5) :: v_arg1
          character(4), dimension(5,5) :: v_arg2
          character(5), dimension(10) :: v_arg3
          character(5), dimension(5,5) :: v_arg4
          character(7), dimension(1:5, -5:1, 10) :: v_arg5

        ! --Other attributes

          character(5), STATIC :: v_arg6(10,10)
          character(6), VOLATILE :: v_arg7(5,5,5)
          character(6), AUTOMATIC :: v_arg8(3,3)
          character(5), TARGET  :: v_arg9(4,4)
          character(6), PARAMETER :: v_arg10(5,5) = "TENS  "
          character(7), TARGET :: v_arg11(10)

         ! b) Deferred Shape array

          character(5), pointer :: v_arg12(:,:)
          character(5), pointer, CONTIGUOUS :: v_arg13(:,:)
          character(:), pointer :: v_arg14(:)
          character(7), ALLOCATABLE, DIMENSION(:, :, :) :: v_arg15
          character(7), ALLOCATABLE :: v_arg16(:)
          character(:), ALLOCATABLE :: v_arg17(:,:)

         ! c) Implied Shape Array (All elements in an array constructor must have the same type and type parameters)

          CHARACTER(5), PARAMETER :: v_arg18(4:*) = ["EIGHTEEN", "EIGHTEEN", "EIGHTEEN", "EIGHTEEN","EIGHTEEN"]
          !"
          CHARACTER(*), PARAMETER :: v_arg19(4:*) = ["NINETEEN", "NINETEEN","NINETEEN", "NINETEEN","NINETEEN"]
          !"
          character(10) v_arg22(10,10,10)
          character(12) v_arg23(10,10,10)

          character(11) v_arg24(10,10,10)
          character(11) v_arg25(10,10,10)
          character(10) v_arg26(10,10)
          character(12) v_arg27(10,10,10)
          character(12) v_arg28(10,10,10)
          character(11) v_arg29(10,10)
          character(6) v_arg30(10,10)

          TYPE BASE
            CHARACTER(10) v_char_base(5,5)
          END TYPE BASE

          TYPE, EXTENDS(BASE) :: EXT_TYPE
            CHARACTER(10) :: v_char_ext_type(3,3,3)
          END TYPE EXT_TYPE

          type(BASE) :: base_vtest
          type(EXT_TYPE) :: ext_vtest

          logical, dimension(5,5) :: v_mask2





        ! Initialization

          v_arg1 = "ONES"
          v_arg2 = "TWOS"
          v_arg3 = "THREES"
          v_arg4 = "FOURS"
          v_arg5 = "FIVES  "
          v_arg6 = "SIXES"
          v_arg7 = "SEVENS"
          v_arg8 = "EIGHTS"
          v_arg9 = "NINES"
          v_arg11 = "ELEVENS"
          v_arg12 => v_arg9
          v_arg13 => v_arg9
          v_arg14 => v_arg11
          allocate(v_arg15(8, -1:3, 5))
          v_arg15 = "FIFTEEN"
          allocate(v_arg16(10))
          v_arg16 = "SIXTEEN"
          allocate(character(len=16) :: v_arg17(10,10))
          v_arg17 = "SEVENTEEN"
          v_arg22 = "TWENTY_TWO"
          v_arg23 = "TWENTY_THREE"
          v_arg24 = "TWENTY_FOUR"
          v_arg25 = "TWENTY_FIVE"
          v_arg26 = "TWENTY_SIX"
          v_arg27 = "TWENTY_SEVEN"
          v_arg28 = "TWENTY_EIGHT"
          v_arg29 = "TWENTY_NINE"
          v_arg30 = "THIRTY"

          base_vtest%v_char_base = "THIRTY_ONE"
          ext_vtest%v_char_ext_type = "THIRTY_TWO"

          v_mask2 = .TRUE.

        ! F2C
        print *, "F2C"

        !a) Character array with different attributes


        !1.--- Explicti shape array
        call check_f_to_c(v_arg1, LEN(v_arg1), 1)
        call check_f_to_c(v_arg2, LEN(v_arg2), 2)
        call check_f_to_c(v_arg3, LEN(v_arg3), 3)
        call check_f_to_c(v_arg4, LEN(v_arg4), 4)
        call check_f_to_c(v_arg5, LEN(v_arg5), 5)
        call check_f_to_c(v_arg6, LEN(v_arg6), 6)
        call check_f_to_c(v_arg7, LEN(v_arg7), 7)
        call check_f_to_c(v_arg8, LEN(v_arg8), 8)
        call check_f_to_c(v_arg9, LEN(v_arg9), 9)
        call check_f_to_c(v_arg10, LEN(v_arg10), 10)
        call check_f_to_c(v_arg11, LEN(v_arg11), 11)
        !2.--- Deferred Shape array
        call check_f_to_c(v_arg12, LEN(v_arg12), 12)
        call check_f_to_c(v_arg13, LEN(v_arg13), 13)
        call check_f_to_c(v_arg14, LEN(v_arg14), 14)
        call check_f_to_c(v_arg15, LEN(v_arg15), 15)
        call check_f_to_c(v_arg16, LEN(v_arg16), 16)
        call check_f_to_c(v_arg17, LEN(v_arg17), 17)
        !3.--- Implied Shape Array
        call check_f_to_c(v_arg18, LEN(v_arg18), 18)
        call check_f_to_c(v_arg19, LEN(v_arg19), 19)
        !4.--- Explicit shape array:automatic
        call SUB20(10, "F2C")
        call SUB21(10, "F2C")
        !5.--- Explicit shape array:adjustable
        call SUB22(3, v_arg22, "F2C")
        call SUB23(12, v_arg23, "F2C")
        !6.--- Assumed shape array
        !call SUB24(v_arg24, "F2C")
        call SUB25(v_arg25, "F2C")
        !7.--- Assumed size array
        call SUB26(v_arg26, LEN(v_arg26), "F2C")
        call SUB27(v_arg27, "F2C")
        call SUB28(v_arg28, "F2C")
        call SUB29(v_arg29, "F2C")
        call SUB30(v_arg30, 6, "F2C")


        !b) array component of a derived type

        call check_f_to_c(base_vtest%v_char_base, LEN(base_vtest%v_char_base), 31)
        call check_f_to_c(ext_vtest%v_char_ext_type, LEN(ext_vtest%v_char_ext_type), 32)

        !c) Array Section

        call check_f_to_c(v_arg3(1:3), LEN(v_arg3(1:3)), 3)
        call check_f_to_c(v_arg8(1:3, 1:3), LEN(v_arg8), 8)

        !d) Function return value array

        call check_f_to_c(func3(), LEN(func3()), 23)
        call check_f_to_c(func1(), LEN(func1()), 24)
        call check_f_to_c(func2("TWENTY_FIVE"), LEN(func2("TWENTY_FIVE")), 25)

        !e) Expressions with character array variables

        call check_f_to_c(MERGE(v_arg2, v_arg2, v_mask2), LEN(MERGE(v_arg2, v_arg2, v_mask2)),2)
        call check_f_to_c((/ 'ONE', 'ONE', 'ONE', 'ONE', 'ONE', 'ONE' /), 3, 1)
        call check_f_to_c(['FOUR', 'FOUR', 'FOUR', 'FOUR', 'FOUR'],4, 4)


      contains

      ! automatic array

       SUBROUTINE SUB20(Y, test_type)
         INTEGER Y
         CHARACTER(6) Z (20, 1:Y)
         CHARACTER(3) test_type
         Z  = "TWENTY"

           call check_f_to_c(Z,LEN(Z),20)



       END SUBROUTINE

       SUBROUTINE SUB21(Y, test_type)
         INTEGER Y
         CHARACTER(Y) Z (5, 1:Y)
         CHARACTER(3) test_type
         Z  = "TWENTY_ONE"

           call check_f_to_c(Z, LEN(Z), 21)



       END SUBROUTINE

       ! adjustable array

       SUBROUTINE SUB22(X, Y, test_type)
         INTEGER X
         CHARACTER(10) Y(X*3)
         CHARACTER(3) test_type

           call check_f_to_c(Y,LEN(Y), 22)



       END SUBROUTINE

       SUBROUTINE SUB23(X, Y, test_type)
         INTEGER X
         CHARACTER(X) Y(X*3)
         CHARACTER(3) test_type

           call check_f_to_c(Y,LEN(Y), 23)



       END SUBROUTINE

       ! assumed shape

       SUBROUTINE SUB24(B, test_type)
         CHARACTER(11) B(1:,:,10:)
         CHARACTER(3) test_type

           call check_f_to_c(B, LEN(B), 24)



       END SUBROUTINE

       SUBROUTINE SUB25(B, test_type)
         CHARACTER(*) B(1:,:,10:)
         CHARACTER(3) test_type

            call check_f_to_c(B, LEN(B), 25)



       END SUBROUTINE

       ! assumed size

       SUBROUTINE SUB26(ARRAY, L, test_type)
         CHARACTER(*) ARRAY(*)
         integer L
         CHARACTER(3) test_type

           call check_f_to_c(ARRAY, L, 26)



       END SUBROUTINE

       SUBROUTINE SUB27(ARRAY, test_type)
         CHARACTER(12) ARRAY(*)
         CHARACTER(3) test_type

           call check_f_to_c(ARRAY, LEN(ARRAY), 27)



       END SUBROUTINE

       SUBROUTINE SUB28(ARRAY, test_type)
         CHARACTER(12) ARRAY(2,*)
         CHARACTER(3) test_type

           call check_f_to_c(ARRAY, LEN(ARRAY), 28)



       END SUBROUTINE

       SUBROUTINE SUB29(ARRAY, test_type)
         CHARACTER(11) ARRAY(2,-4:*)
         CHARACTER(3) test_type

           call check_f_to_c(ARRAY, LEN(ARRAY), 29)



       END SUBROUTINE

       SUBROUTINE SUB30(ARRAY, L, test_type)
         CHARACTER(*) ARRAY(2,-4:*)
         CHARACTER(3) test_type
         integer L

           call check_f_to_c(ARRAY, LEN(ARRAY), 30)



       END SUBROUTINE

       ! Other functions

       FUNCTION func3() RESULT (v_char)
          character(12) v_char(10)
          v_char = "TWENTY_THREE"
       END FUNCTION func3
      end program

       FUNCTION func1 () RESULT (v_char)
          character(11) v_char(10)
          v_char = "TWENTY_FOUR"
       END FUNCTION func1

       FUNCTION func2 (c_arg) RESULT (v_char)
          character(11) :: v_char(10)
          character(*), DIMENSION(1) :: c_arg
          v_char = c_arg(1)
       END FUNCTION func2









