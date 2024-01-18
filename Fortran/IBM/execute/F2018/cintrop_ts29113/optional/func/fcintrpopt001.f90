! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt001.f
!*
!* PROGRAMMER                   : Ying Zhang
!* DATE                         : June 25, 2012
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from C, where the procedure is defined in Fortran
!*
!* Actual Argument:
!* 	NULL Pointer, or corresponding C types
!*
!* Dummy Argument:
!*	all intrinsic types listed in table above
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      subroutine sub_int(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_short(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_short), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_long(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_long), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_long_long(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_long_long), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_unsigned(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_signed_char), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_size_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_size_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int8_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int8_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int16_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int16_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int32_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int32_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int64_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int64_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_least8_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_least8_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_least16_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_least16_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_least32_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_least32_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_least64_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_least64_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_fast8_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_fast8_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_fast16_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_fast16_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_fast32_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_fast32_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_int_fast64_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int_fast64_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_intmax_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_intmax_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_intptr_t(arg) bind(C)
        use iso_c_binding
        implicit none
        integer(c_intptr_t), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_float(arg) bind(C)
        use iso_c_binding
        implicit none
        real(c_float), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end


      subroutine sub_double(arg) bind(C)
        use iso_c_binding
        implicit none
        real(c_double), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_long_double(arg) bind(C)
        use iso_c_binding
        implicit none
        real(c_long_double), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_float_complex(arg) bind(C)
        use iso_c_binding
        implicit none
        complex(c_float_complex), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_double_complex(arg) bind(C)
        use iso_c_binding
        implicit none
        complex(c_double_complex), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_long_double_complex(arg) bind(C)
        use iso_c_binding
        implicit none
        complex(c_long_double_complex), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_bool(arg) bind(C)
        use iso_c_binding
        implicit none
        logical(c_bool), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

      subroutine sub_char(arg) bind(C)
        use iso_c_binding
        implicit none
        character(c_char), optional :: arg

        if (present(arg)) then
           print *, arg
        else
           print *, "arg is not present"
        end if
      end

