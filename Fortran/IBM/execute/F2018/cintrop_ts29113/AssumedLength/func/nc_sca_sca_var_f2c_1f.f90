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
            character(*) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_c_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
        end interface

        TYPE BASE
          CHARACTER(11) v_char_base
        END TYPE BASE

        TYPE, EXTENDS(BASE) :: EXT_TYPE
          CHARACTER(12) :: v_char_ext_type
        END TYPE EXT_TYPE

        CHARACTER(LEN=3, KIND=1) v_char1 /'ONE'/
        CHARACTER(3,1) v_char2 /'TWO_'/
        !CHARACTER(268435456) v_char3 /'TEST BIG'/
        CHARACTER(4) v_char4 /'FOUR'/
        CHARACTER(KIND=1) v_char5 /'FIVE'/
        CHARACTER(LEN=3) v_char6 /'SIX'/
        CHARACTER(5) v_char7 /'SEVEN'/
        CHARACTER*5 v_char8  /'EIGHT'/

        CHARACTER(LEN=*, KIND=1), PARAMETER :: v_char9 = 'NINE'
        CHARACTER(*,1), PARAMETER :: v_char10 ='TEN'
        CHARACTER(*), PARAMETER :: v_char11 ='ELEVEN'

        CHARACTER(LEN=:, KIND=1), ALLOCATABLE :: v_char12
        CHARACTER(10), ASYNCHRONOUS :: v_char13 /'THIRTEEN'/
        CHARACTER(8), AUTOMATIC :: v_char14
        CHARACTER(LEN=:, KIND=1), POINTER :: v_char15
        CHARACTER(10), SAVE :: v_char16 /'SIXTEEN'/
        CHARACTER(9), STATIC :: v_char17 /'SEVENTEEN'/
        CHARACTER(8), TARGET :: v_char18 /'EIGHTEEN'/
        CHARACTER(8), VOLATILE :: v_char19 /'NINETEEN'/

        type(BASE) :: base_vtest
        type(EXT_TYPE) :: ext_vtest

        CHARACTER(6), DIMENSION(1) :: v_char20 /'TWENTY'/

        CHARACTER(11), EXTERNAL :: func1
        CHARACTER(11), EXTERNAL :: func2
        CHARACTER(10), INTRINSIC :: TRIM


        base_vtest%v_char_base = "TWENTY_ONE"
        ext_vtest%v_char_ext_type = "TWENTY_TWO"

        allocate(character(len=12) :: v_char12)
        v_char12 = "TWELVE"
        v_char14 = "FOURTEEN"
        v_char15 => v_char18


        ! F2C

        !a) Character variables with different attributes

        print *, "Character variables with different attributes Test"

        call check_f_to_c(v_char1, LEN(v_char1), 1)
        call check_f_to_c(v_char2, LEN(v_char2), 2)
        !!call check_f_to_c(v_char3, LEN(v_char3), 3)  ! Tseting for big variable
        call check_f_to_c(v_char4, LEN(v_char4), 4)
        call check_f_to_c(v_char5, LEN(v_char5), 5)
        call check_f_to_c(v_char6, LEN(v_char6), 6)
        call check_f_to_c(v_char7, LEN(v_char7), 7)
        call check_f_to_c(v_char8, LEN(v_char8), 8)
        call check_f_to_c(v_char9, LEN(v_char9), 9)
        call check_f_to_c(v_char10, LEN(v_char10), 10)
        call check_f_to_c(v_char11, LEN(v_char11), 11)
        call check_f_to_c(v_char12, LEN(v_char12), 12)
        call check_f_to_c(v_char13, LEN(v_char13), 13)
        call check_f_to_c(v_char14, LEN(v_char14), 14)
        call check_f_to_c(v_char15, LEN(v_char15), 15)
        call check_f_to_c(v_char16, LEN(v_char16), 16)
        call check_f_to_c(v_char17, LEN(v_char17), 17)
        call check_f_to_c(v_char18, LEN(v_char18), 18)
        call check_f_to_c(v_char19, LEN(v_char19), 19)

        !b) Scalar component of a derived type

        call check_f_to_c(base_vtest%v_char_base, LEN(base_vtest%v_char_base), 21)
        call check_f_to_c(ext_vtest%v_char_ext_type, LEN(ext_vtest%v_char_ext_type), 22)
        call check_f_to_c(ext_vtest%v_char_ext_type//base_vtest%v_char_base, LEN(ext_vtest%v_char_ext_type//base_vtest%v_char_base), 31)

        !c) Scalar array element

        call check_f_to_c(v_char20(1), LEN(v_char20(1)), 20)
        call check_f_to_c(v_char20(1)//"", LEN(v_char20(1)//""), 29)
        call check_f_to_c(v_char20(1)//v_char20(1), LEN(v_char20(1)//v_char20(1)), 30)

        !d) Function return value

        call check_f_to_c(func3(), LEN(func3()), 23)
        call check_f_to_c(func1(), LEN(func1()), 24)
        call check_f_to_c(func2("TWENTY_FIVE"), LEN(func2("TWENTY_FIVE")), 25)

        call check_f_to_c(TRIM(v_char12), LEN(TRIM(v_char12)), 26)  !ICE
        call check_f_to_c(TRIM(v_char12//"__"), LEN(TRIM(v_char12//"__")), 27)

        !e) Expressions with character variables

        call check_f_to_c(v_char12//v_char12, LEN(v_char12//v_char12), 28)

        ! F2F

        !a) Character variables with different attributes

        print *, "Character variables with different attributes Test"

        call check_f_to_f(v_char1, LEN(v_char1), 1)
        call check_f_to_f(v_char2, LEN(v_char2), 2)
        !!call check_f_to_f(v_char3, LEN(v_char3), 3)  ! Tseting for big variable
        call check_f_to_f(v_char4, LEN(v_char4), 4)
        call check_f_to_f(v_char5, LEN(v_char5), 5)
        call check_f_to_f(v_char6, LEN(v_char6), 6)
        call check_f_to_f(v_char7, LEN(v_char7), 7)
        call check_f_to_f(v_char8, LEN(v_char8), 8)
        call check_f_to_f(v_char9, LEN(v_char9), 9)
        call check_f_to_f(v_char10, LEN(v_char10), 10)
        call check_f_to_f(v_char11, LEN(v_char11), 11)
        call check_f_to_f(v_char12, LEN(v_char12), 12)
        call check_f_to_f(v_char13, LEN(v_char13), 13)
        call check_f_to_f(v_char14, LEN(v_char14), 14)
        call check_f_to_f(v_char15, LEN(v_char15), 15)
        call check_f_to_f(v_char16, LEN(v_char16), 16)
        call check_f_to_f(v_char17, LEN(v_char17), 17)
        call check_f_to_f(v_char18, LEN(v_char18), 18)
        call check_f_to_f(v_char19, LEN(v_char19), 19)

        !b) Scalar component of a derived type

        call check_f_to_f(base_vtest%v_char_base, LEN(base_vtest%v_char_base), 21)
        call check_f_to_f(ext_vtest%v_char_ext_type, LEN(ext_vtest%v_char_ext_type), 22)
        call check_f_to_f(ext_vtest%v_char_ext_type//base_vtest%v_char_base, LEN(ext_vtest%v_char_ext_type//base_vtest%v_char_base), 31)

        !c) Scalar array element

        call check_f_to_f(v_char20(1), LEN(v_char20(1)), 20)
        call check_f_to_f(v_char20(1)//"", LEN(v_char20(1)//""), 29)
        call check_f_to_f(v_char20(1)//v_char20(1), LEN(v_char20(1)//v_char20(1)), 30)

        !d) Function return value

        call check_f_to_f(func3(), LEN(func3()), 23)
        call check_f_to_f(func1(), LEN(func1()), 24)
        call check_f_to_f(func2("TWENTY_FIVE"), LEN(func2("TWENTY_FIVE")), 25)

        call check_f_to_f(TRIM(v_char12), LEN(TRIM(v_char12)), 26)  !ICE
        call check_f_to_f(TRIM(v_char12//"__"), LEN(TRIM(v_char12//"__")), 27)

        !e) Expressions with character variables

        call check_f_to_f(v_char12//v_char12, LEN(v_char12//v_char12), 28)

        ! F2F2C

        !a) Character variables with different attributes

        print *, "Character variables with different attributes Test"

        call check_f_to_f_to_c(v_char1, LEN(v_char1), 1)
        call check_f_to_f_to_c(v_char2, LEN(v_char2), 2)
        !!call check_f_to_f_to_c(v_char3, LEN(v_char3), 3)  ! Tseting for big variable
        call check_f_to_f_to_c(v_char4, LEN(v_char4), 4)
        call check_f_to_f_to_c(v_char5, LEN(v_char5), 5)
        call check_f_to_f_to_c(v_char6, LEN(v_char6), 6)
        call check_f_to_f_to_c(v_char7, LEN(v_char7), 7)
        call check_f_to_f_to_c(v_char8, LEN(v_char8), 8)
        call check_f_to_f_to_c(v_char9, LEN(v_char9), 9)
        call check_f_to_f_to_c(v_char10, LEN(v_char10), 10)
        call check_f_to_f_to_c(v_char11, LEN(v_char11), 11)
        call check_f_to_f_to_c(v_char12, LEN(v_char12), 12)
        call check_f_to_f_to_c(v_char13, LEN(v_char13), 13)
        call check_f_to_f_to_c(v_char14, LEN(v_char14), 14)
        call check_f_to_f_to_c(v_char15, LEN(v_char15), 15)
        call check_f_to_f_to_c(v_char16, LEN(v_char16), 16)
        call check_f_to_f_to_c(v_char17, LEN(v_char17), 17)
        call check_f_to_f_to_c(v_char18, LEN(v_char18), 18)
        call check_f_to_f_to_c(v_char19, LEN(v_char19), 19)


        !b) Scalar component of a derived type

        call check_f_to_f_to_c(base_vtest%v_char_base, LEN(base_vtest%v_char_base), 21)
        call check_f_to_f_to_c(ext_vtest%v_char_ext_type, LEN(ext_vtest%v_char_ext_type), 22)
        call check_f_to_f_to_c(ext_vtest%v_char_ext_type//base_vtest%v_char_base, LEN(ext_vtest%v_char_ext_type//base_vtest%v_char_base), 31)

        !c) Scalar array element

        call check_f_to_f_to_c(v_char20(1), LEN(v_char20(1)), 20)
        call check_f_to_f_to_c(v_char20(1)//"", LEN(v_char20(1)//""), 29)
        call check_f_to_f_to_c(v_char20(1)//v_char20(1), LEN(v_char20(1)//v_char20(1)), 30)

        !d) Function return value

        call check_f_to_f_to_c(func3(), LEN(func3()), 23)
        call check_f_to_f_to_c(func1(), LEN(func1()), 24)
        call check_f_to_f_to_c(func2("TWENTY_FIVE"), LEN(func2("TWENTY_FIVE")), 25)
        !"

        call check_f_to_f_to_c(TRIM(v_char12), LEN(TRIM(v_char12)), 26)  !ICE
        call check_f_to_f_to_c(TRIM(v_char12//"__"), LEN(TRIM(v_char12//"__")), 27)

        !e) Expressions with character variables

        call check_f_to_f_to_c(v_char12//v_char12, LEN(v_char12//v_char12), 28)



        ! F2F2F

        !a) Character variables with different attributes

        print *, "Character variables with different attributes Test"

        call check_f_to_f_to_f(v_char1, LEN(v_char1), 1)
        call check_f_to_f_to_f(v_char2, LEN(v_char2), 2)
        !!call check_f_to_f_to_f(v_char3, LEN(v_char3), 3)  ! Tseting for big variable
        call check_f_to_f_to_f(v_char4, LEN(v_char4), 4)
        call check_f_to_f_to_f(v_char5, LEN(v_char5), 5)
        call check_f_to_f_to_f(v_char6, LEN(v_char6), 6)
        call check_f_to_f_to_f(v_char7, LEN(v_char7), 7)
        call check_f_to_f_to_f(v_char8, LEN(v_char8), 8)
        call check_f_to_f_to_f(v_char9, LEN(v_char9), 9)
        call check_f_to_f_to_f(v_char10, LEN(v_char10), 10)
        call check_f_to_f_to_f(v_char11, LEN(v_char11), 11)
        call check_f_to_f_to_f(v_char12, LEN(v_char12), 12)
        call check_f_to_f_to_f(v_char13, LEN(v_char13), 13)
        call check_f_to_f_to_f(v_char14, LEN(v_char14), 14)
        call check_f_to_f_to_f(v_char15, LEN(v_char15), 15)
        call check_f_to_f_to_f(v_char16, LEN(v_char16), 16)
        call check_f_to_f_to_f(v_char17, LEN(v_char17), 17)
        call check_f_to_f_to_f(v_char18, LEN(v_char18), 18)
        call check_f_to_f_to_f(v_char19, LEN(v_char19), 19)


        !b) Scalar component of a derived type

        call check_f_to_f_to_f(base_vtest%v_char_base, LEN(base_vtest%v_char_base), 21)
        call check_f_to_f_to_f(ext_vtest%v_char_ext_type, LEN(ext_vtest%v_char_ext_type), 22)
        call check_f_to_f_to_f(ext_vtest%v_char_ext_type//base_vtest%v_char_base, LEN(ext_vtest%v_char_ext_type//base_vtest%v_char_base), 31)

        !c) Scalar array element

        call check_f_to_f_to_f(v_char20(1), LEN(v_char20(1)), 20)
        call check_f_to_f_to_f(v_char20(1)//"", LEN(v_char20(1)//""), 29)
        call check_f_to_f_to_f(v_char20(1)//v_char20(1), LEN(v_char20(1)//v_char20(1)), 30)

        !d) Function return value

        call check_f_to_f_to_f(func3(), LEN(func3()), 23)
        call check_f_to_f_to_f(func1(), LEN(func1()), 24)
        call check_f_to_f_to_f(func2("TWENTY_FIVE"), LEN(func2("TWENTY_FIVE")), 25)
        !"

        call check_f_to_f_to_f(TRIM(v_char12), LEN(TRIM(v_char12)), 26)  !ICE
        call check_f_to_f_to_f(TRIM(v_char12//"__"), LEN(TRIM(v_char12//"__")), 27)

        !e) Expressions with character variables

        call check_f_to_f_to_f(v_char12//v_char12, LEN(v_char12//v_char12), 28)



        ! F2C2F

        !a) Character variables with different attributes

        print *, "Character variables with different attributes Test"

        call check_f_to_c_to_f(v_char1, LEN(v_char1), 1)
        call check_f_to_c_to_f(v_char2, LEN(v_char2), 2)
        !!call check_f_to_c_to_f(v_char3, LEN(v_char3), 3)  ! Tseting for big variable
        call check_f_to_c_to_f(v_char4, LEN(v_char4), 4)
        call check_f_to_c_to_f(v_char5, LEN(v_char5), 5)
        call check_f_to_c_to_f(v_char6, LEN(v_char6), 6)
        call check_f_to_c_to_f(v_char7, LEN(v_char7), 7)
        call check_f_to_c_to_f(v_char8, LEN(v_char8), 8)
        call check_f_to_c_to_f(v_char9, LEN(v_char9), 9)
        call check_f_to_c_to_f(v_char10, LEN(v_char10), 10)
        call check_f_to_c_to_f(v_char11, LEN(v_char11), 11)
        call check_f_to_c_to_f(v_char12, LEN(v_char12), 12)
        call check_f_to_c_to_f(v_char13, LEN(v_char13), 13)
        call check_f_to_c_to_f(v_char14, LEN(v_char14), 14)
        call check_f_to_c_to_f(v_char15, LEN(v_char15), 15)
        call check_f_to_c_to_f(v_char16, LEN(v_char16), 16)
        call check_f_to_c_to_f(v_char17, LEN(v_char17), 17)
        call check_f_to_c_to_f(v_char18, LEN(v_char18), 18)
        call check_f_to_c_to_f(v_char19, LEN(v_char19), 19)


        !b) Scalar component of a derived type

        call check_f_to_c_to_f(base_vtest%v_char_base, LEN(base_vtest%v_char_base), 21)
        call check_f_to_c_to_f(ext_vtest%v_char_ext_type, LEN(ext_vtest%v_char_ext_type), 22)
        call check_f_to_c_to_f(ext_vtest%v_char_ext_type//base_vtest%v_char_base, LEN(ext_vtest%v_char_ext_type//base_vtest%v_char_base), 31)

        !c) Scalar array element

        call check_f_to_c_to_f(v_char20(1), LEN(v_char20(1)), 20)
        call check_f_to_c_to_f(v_char20(1)//"", LEN(v_char20(1)//""), 29)
        call check_f_to_c_to_f(v_char20(1)//v_char20(1), LEN(v_char20(1)//v_char20(1)), 30)

        !d) Function return value

        call check_f_to_c_to_f(func3(), LEN(func3()), 23)
        call check_f_to_c_to_f(func1(), LEN(func1()), 24)
        call check_f_to_c_to_f(func2("TWENTY_FIVE"), LEN(func2("TWENTY_FIVE")), 25)
        !"

        print *, v_char12
        print *, LEN(TRIM(v_char12))
        call check_f_to_c_to_f(TRIM(v_char12), LEN(TRIM(v_char12)), 26)  !ICE
        call check_f_to_c_to_f(TRIM(v_char12//"__"), LEN(TRIM(v_char12//"__")), 27)

        !e) Expressions with character variables

        call check_f_to_c_to_f(v_char12//v_char12, LEN(v_char12//v_char12), 28)



      contains
       FUNCTION func3() RESULT (v_char)
          character(7) v_char
          v_char = "TWENTY_THREE"
       END FUNCTION func3
      end program

      subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        if(c_len .NE. LEN(c_arg2)) then
           error STOP 1
        endif
        if(RANK(c_arg2) .NE. 0) then
           error STOP 2
        endif
        if(test_no .EQ. 1) then
          c_test  = "ONE"
        endif
        if(test_no .EQ. 2) then
          c_test  = "TWO"
        endif
        if(test_no .EQ. 3) then
          c_test  = "THREE"
        endif
        if(test_no .EQ. 4) then
          c_test  = "FOUR"
        endif
        if(test_no .EQ. 5) then
          c_test  = "FIVE"
        endif
        if(test_no .EQ. 6) then
          c_test  = "SIX"
        endif
        if(test_no .EQ. 7) then
          c_test  = "SEVEN"
        endif
        if(test_no .EQ. 8) then
          c_test  = "EIGHT"
        endif
        if(test_no .EQ. 9) then
          c_test  = "NINE"
        endif
        if(test_no .EQ. 10) then
          c_test  = "TEN"
        endif
        if(test_no .EQ. 11) then
          c_test  = "ELEVEN"
        endif
        if(test_no .EQ. 12) then
          c_test  = "TWELVE"
        endif
        if(test_no .EQ. 13) then
          c_test  = "THIRTEEN  "
        endif
        if(test_no .EQ. 14) then
          c_test  = "FOURTEEN"
        endif
        if(test_no .EQ. 15) then
          c_test  = "EIGHTEEN"
        endif
        if(test_no .EQ. 16) then
          c_test  = "SIXTEEN   "
        endif
        if(test_no .EQ. 17) then
          c_test  = "SEVENTEEN"
        endif
        if(test_no .EQ. 18) then
          c_test  = "EIGHTEEN"
        endif
        if(test_no .EQ. 19) then
          c_test  = "NINETEEN"
        endif
        if(test_no .EQ. 20) then
        c_test = "TWENTY"
        endif
	if(test_no .EQ. 21) then
        c_test = "TWENTY_ONE "
       endif
	if(test_no .EQ. 22) then
        c_test = "TWENTY_TWO  "
       endif
	if(test_no .EQ. 23) then
        c_test = "TWENTY_THREE"
       endif
	if(test_no .EQ. 24) then
        c_test = "TWENTY_FOUR"
       endif
	if(test_no .EQ. 25) then
        c_test = "TWENTY_FIVE"
       endif
	if(test_no .EQ. 26) then
        c_test = "TWELVE"
       endif
	if(test_no .EQ. 27) then
        c_test = "TWELVE__"
       endif
	if(test_no .EQ. 28) then
        c_test = "TWELVETWELVE"
       endif
	if(test_no .EQ. 29) then
        c_test = "TWENTY "
       endif
	if(test_no .EQ. 30) then
        c_test = "TWENTYTWENTY"
       endif
	if(test_no .EQ. 31) then
        c_test = "TWENTY_TWO  TWENTY_ONE "
       endif
       if(c_arg2 .NE. c_test) then
           error STOP 3
        endif
       end subroutine

      subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
        end interface
        character(*) :: c_arg3
        integer(C_INT) c_len, test_no
        call check_f_to_c(c_arg3,LEN(c_arg3), test_no)
       end subroutine

       subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine
        end interface
        character(*) :: c_arg3
        integer(C_INT) c_len, test_no
        call check_f_to_f(c_arg3,LEN(c_arg3), test_no)
       end subroutine

       FUNCTION func1 () RESULT (v_char)
          character(11) v_char
          v_char = "TWENTY_FOUR"
       END FUNCTION func1

       FUNCTION func2 (c_arg) RESULT (v_char)
          character(11) :: v_char
          character(*) :: c_arg
          v_char = c_arg
       END FUNCTION func2









