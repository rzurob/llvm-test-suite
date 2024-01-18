! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Aug 31, 2015
!*
!* PRIMARY FUNCTIONS TESTED     : F2008 DO CONCURENT
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from Fortran in a DO CONCURRENT construct,
!* (where the procedure is defined in C).
!* Based on cintrop_ts29113/optional/func/fcintrpopt200.scenario
!*
!* Actual Argument:
!*   When have multiple intent(in) dummy arguments in a integer(c_int) function or pure integer(c_int) function, supply
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

integer(c_int) tmp

interface

   pure integer(c_int) function c_func_int(arg1) bind(c)
    import
    integer(c_int), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_short(arg1) bind(c)
    import
    integer(c_short), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_long(arg1) bind(c)
    import
    integer(c_long), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_long_long(arg1) bind(c)
    import
    integer(c_long_long), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_unsigned(arg1) bind(c)
    import
    integer(c_signed_char), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_size_t(arg1) bind(c)
    import
    integer(c_size_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_int8_t(arg1) bind(c)
    import
    integer(c_int8_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_int16_t(arg1) bind(c)
    import
    integer(c_int16_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_int32_t(arg1) bind(c)
    import
    integer(c_int32_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_int64_t(arg1) bind(c)
    import
    integer(c_int64_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_least8_t(arg1) bind(c)
    import
    integer(c_int_least8_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_least16_t(arg1) bind(c)
    import
    integer(c_int_least16_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_least32_t(arg1) bind(c)
    import
    integer(c_int_least32_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_least64_t(arg1) bind(c)
    import
    integer(c_int_least64_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_fast8_t(arg1) bind(c)
    import
    integer(c_int_fast8_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_fast16_t(arg1) bind(c)
    import
    integer(c_int_fast16_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_fast32_t(arg1) bind(c)
    import
    integer(c_int_fast32_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_fast64_t(arg1) bind(c)
    import
    integer(c_int_fast64_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_intmax_t(arg1) bind(c)
    import
    integer(c_intmax_t), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_intptr_t(arg1) bind(c)
    import
    integer(c_intptr_t), intent(in):: arg1
   end

   pure integer(c_int) function c_func_float(arg1) bind(c)
    import
    real(c_float), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_double(arg1) bind(c)
    import
    real(c_double), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_long_double(arg1) bind(c)
    import
    real(c_long_double), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_float_complex(arg1) bind(c)
    import
    complex(c_float_complex), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_double_complex(arg1) bind(c)
    import
    complex(c_double_complex), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_long_double_complex(arg1) bind(c)
    import
    complex(c_long_double_complex), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_bool(arg1) bind(c)
    import
    logical(c_bool), intent(in) :: arg1
   end

   pure integer(c_int) function c_func_char(arg1) bind(c)
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
      tmp =  c_func_int(i1)
      if (tmp .eq. 0) error stop 1
    else
      tmp =  c_func_short(i2)
      if (tmp .eq. 0) error stop 2
    end if
  else
    if (j .eq. 1) then
      tmp =  c_func_long(i3)
      if (tmp .eq. 0) error stop 3
    else
      tmp =  c_func_long_long(i4)
      if (tmp .eq. 0) error stop 4
    end if
  end if
end do

do concurrent (i=1:12)
  do concurrent (j=1:2)
    select case(i)
      case(1)
        if(j .eq. 1) then
          tmp =  c_func_unsigned(i5)
          if (tmp .eq. 0) error stop 5
        else
          tmp =  c_func_size_t(i6)
          if (tmp .eq. 0) error stop 6
        end if
      case(2)
        if(j .eq. 1) then
          tmp =  c_func_int8_t(i7)
          if (tmp .eq. 0) error stop 7
        else
          tmp =  c_func_int16_t(i8)
          if (tmp .eq. 0) error stop 8
        end if
      case(3)
        if(j .eq. 1) then
          tmp =  c_func_int32_t(i9)
          if (tmp .eq. 0) error stop 9
        else
          tmp =  c_func_int64_t(i10)
          if (tmp .eq. 0) error stop 10
        end if
      case(4)
        if(j .eq. 1) then
          tmp =  c_func_least8_t(i11)
          if (tmp .eq. 0) error stop 11
        else
          tmp =  c_func_least16_t(i12)
          if (tmp .eq. 0) error stop 12
        end if
      case(5)
        if(j .eq. 1) then
          tmp =  c_func_least32_t(i13)
          if (tmp .eq. 0) error stop 13
        else
          tmp =  c_func_least64_t(i14)
          if (tmp .eq. 0) error stop 14
        end if
      case(6)
        if(j .eq. 1) then
          tmp =  c_func_fast8_t(i15)
          if (tmp .eq. 0) error stop 15
        else
          tmp =  c_func_fast16_t(i16)
          if (tmp .eq. 0) error stop 16
        end if
      case(7)
        if(j .eq. 1) then
          tmp =  c_func_fast32_t(i17)
          if (tmp .eq. 0) error stop 17
        else
          tmp =  c_func_fast64_t(i18)
          if (tmp .eq. 0) error stop 18
        end if
      case(8)
        if(j .eq. 1) then
          tmp =  c_func_intmax_t(i19)
          if (tmp .eq. 0) error stop 19
        else
          tmp =  c_func_intptr_t(i20)
          if (tmp .eq. 0) error stop 20
        end if
      case(9)
        if(j .eq. 1) then
          tmp =  c_func_float(r1)
          if (tmp .eq. 0) error stop 21
        else
          tmp =  c_func_double(r2)
          if (tmp .eq. 0) error stop 22
        end if
      case(10)
        if(j .eq. 1) then
          tmp =  c_func_long_double(r3)
          if (tmp .eq. 0) error stop 23
        else
          tmp =  c_func_float_complex(c1)
          if (tmp .eq. 0) error stop 24
        end if
      case(11)
        if(j .eq. 1) then
          tmp =  c_func_double_complex(c2)
          if (tmp .eq. 0) error stop 25
        else
          tmp =  c_func_long_double_complex(c3)
          if (tmp .eq. 0) error stop 26
        end if
      case(12)
        if(j .eq. 1) then
          tmp =  c_func_bool(ll)
          if (tmp .eq. 0) error stop 27
        else
          tmp =  c_func_char(cc)
          if (tmp .eq. 0) error stop 28
        end if
    end select
  end do
end do

end


