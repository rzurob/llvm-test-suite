! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt200.f
!*
!* PROGRAMMER                   : Bernard Kan
!* DATE                         : Aug 31, 2015
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008 DO CONCURENT
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from Fortran in a DO CONCURRENT construct, 
!* (where the procedure is defined in C).
!* Based on cintrop_ts29113/optional/func/fcintrpopt200.scenario
!*
!* Actual Argument:
!*   When have multiple intent(in) dummy arguments in a function or pure subroutine, supply
!*   with no actual argument, or actual arguments for all the intent(in) ones, or
!*   actual arguments for some of the intent(in) arguments
!*
!* Dummy Argument:
!*  all the interoperable C types from above table
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  15/08/31    BK     - modified to work with do concurrent
!*  12/06/14    YZ     - Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

use iso_c_binding
implicit none

interface

   pure subroutine c_func_int(arg1) bind(c)
    import
    integer(c_int), intent(in) :: arg1
   end

   pure subroutine c_func_short(arg1) bind(c)
    import
    integer(c_short), intent(in) :: arg1
   end

   pure subroutine c_func_long(arg1) bind(c)
    import
    integer(c_long), intent(in) :: arg1
   end

   pure subroutine c_func_long_long(arg1) bind(c)
    import
    integer(c_long_long), intent(in) :: arg1
   end

   pure subroutine c_func_unsigned(arg1) bind(c)
    import
    integer(c_signed_char), intent(in) :: arg1
   end

   pure subroutine c_func_size_t(arg1) bind(c)
    import
    integer(c_size_t), intent(in) :: arg1
   end

   pure subroutine c_func_int8_t(arg1) bind(c)
    import
    integer(c_int8_t), intent(in) :: arg1
   end

   pure subroutine c_func_int16_t(arg1) bind(c)
    import
    integer(c_int16_t), intent(in) :: arg1
   end

   pure subroutine c_func_int32_t(arg1) bind(c)
    import
    integer(c_int32_t), intent(in) :: arg1
   end

   pure subroutine c_func_int64_t(arg1) bind(c)
    import
    integer(c_int64_t), intent(in) :: arg1
   end

   pure subroutine c_func_least8_t(arg1) bind(c)
    import
    integer(c_int_least8_t), intent(in) :: arg1
   end

   pure subroutine c_func_least16_t(arg1) bind(c)
    import
    integer(c_int_least16_t), intent(in) :: arg1
   end

   pure subroutine c_func_least32_t(arg1) bind(c)
    import
    integer(c_int_least32_t), intent(in) :: arg1
   end

   pure subroutine c_func_least64_t(arg1) bind(c)
    import
    integer(c_int_least64_t), intent(in) :: arg1
   end

   pure subroutine c_func_fast8_t(arg1) bind(c)
    import
    integer(c_int_fast8_t), intent(in) :: arg1
   end

   pure subroutine c_func_fast16_t(arg1) bind(c)
    import
    integer(c_int_fast16_t), intent(in) :: arg1
   end

   pure subroutine c_func_fast32_t(arg1) bind(c)
    import
    integer(c_int_fast32_t), intent(in) :: arg1
   end

   pure subroutine c_func_fast64_t(arg1) bind(c)
    import
    integer(c_int_fast64_t), intent(in) :: arg1
   end

   pure subroutine c_func_intmax_t(arg1) bind(c)
    import
    integer(c_intmax_t), intent(in) :: arg1
   end

   pure subroutine c_func_intptr_t(arg1) bind(c)
    import
    integer(c_intptr_t), intent(in):: arg1
   end

   pure subroutine c_func_float(arg1) bind(c)
    import
    real(c_float), intent(in) :: arg1
   end

   pure subroutine c_func_double(arg1) bind(c)
    import
    real(c_double), intent(in) :: arg1
   end

   pure subroutine c_func_long_double(arg1) bind(c)
    import
    real(c_long_double), intent(in) :: arg1
   end

   pure subroutine c_func_float_complex(arg1) bind(c)
    import
    complex(c_float_complex), intent(in) :: arg1
   end

   pure subroutine c_func_double_complex(arg1) bind(c)
    import
    complex(c_double_complex), intent(in) :: arg1
   end

   pure subroutine c_func_long_double_complex(arg1) bind(c)
    import
    complex(c_long_double_complex), intent(in) :: arg1
   end

   pure subroutine c_func_bool(arg1) bind(c)
    import
    logical(c_bool), intent(in) :: arg1
   end

   pure subroutine c_func_char(arg1) bind(c)
    import
    character(c_char), intent(in) :: arg1
   end

end interface

    integer(c_int) ::               i1
    integer(c_short) ::             i2
    integer(c_long) ::              i3
    integer(c_long_long) ::         i4
    integer(c_signed_char) ::       i5
    integer(c_size_t) ::            i6
    integer(c_int8_t) ::            i7
    integer(c_int16_t) ::           i8
    integer(c_int32_t) ::           i9
    integer(c_int64_t) ::           i10
    integer(c_int_least8_t) ::      i11
    integer(c_int_least16_t) ::     i12
    integer(c_int_least32_t) ::     i13
    integer(c_int_least64_t) ::     i14
    integer(c_int_fast8_t) ::       i15
    integer(c_int_fast16_t) ::      i16
    integer(c_int_fast32_t) ::      i17
    integer(c_int_fast64_t) ::      i18
    integer(c_intmax_t) ::          i19
    integer(c_intptr_t) ::          i20
    real(c_float) ::                r1
    real(c_double) ::               r2
    real(c_long_double) ::          r3
    complex(c_float_complex) ::     c1
    complex(c_double_complex) ::    c2
    complex(c_long_double_complex)::        c3
    logical(c_bool) ::              ll
    character(c_char) ::            cc
    integer :: i, j
   
    i1=1 
    i2=2 
    i3=3 
    i4=4_C_LONG_LONG
    i5=5 
    i6=6
    i7=7
    i8=8
    i9=9
    i10=10
    i11=11
    i12=12
    i13=13
    i14=14
    i15=15
    i16=16
    i17=17
    i18=18
    i19=19
    i20=20
    r1=3.0
    r2=4.0
    r3=5.0
    c1=(5.0e0,5.0e0)
    c2=(10.0d0,10.0d0)
    c3=(15.0d0,15.0d0)
    ll=.true.
    cc='a'
do concurrent (i=1:2,j=1:2)
  if(i .eq. 1) then
    if (j .eq. 1) then
      call c_func_int(i1)
    else
      call c_func_short(i2)
    end if
  else
    if (j .eq. 1) then
      call c_func_long(i3)
    else
      call c_func_long_long(i4)
    end if
  end if
end do
      call c_func_unsigned(i5)
      call c_func_size_t(i6)
      call c_func_int8_t(i7)
      call c_func_int16_t(i8)
      call c_func_int32_t(i9)
      call c_func_int64_t(i10)
      call c_func_least8_t(i11)
      call c_func_least16_t(i12)
      call c_func_least32_t(i13)
      call c_func_least64_t(i14)
      call c_func_fast8_t(i15)
      call c_func_fast16_t(i16)
      call c_func_fast32_t(i17)
      call c_func_fast64_t(i18)
      call c_func_intmax_t(i19)
      call c_func_intptr_t(i20)
      call c_func_float(r1)
      call c_func_double(r2)
      call c_func_long_double(r3)
      call c_func_float_complex(c1)
      call c_func_double_complex(c2)
      call c_func_long_double_complex(c3)
      call c_func_bool(ll)
      call c_func_char(cc)

end


