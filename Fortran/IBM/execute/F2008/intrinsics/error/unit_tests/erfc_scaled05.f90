program erfc_scaled05
real(8) :: x, y, z
interface
  logical function precision_range_r8(x, y, z)
    real(8) x, y, z
  end function
end interface

x = 0.05d0
do while (x <= 20.05d0)
  y = exp(x**2) * erfc(x)
  z = erfc_scaled(x)
  if (.not. precision_range_r8(y, z, 0.0000000000001d0)) then
    print *, 'x=', x
    print *, 'y=', y
    print *, 'z=', z
    error stop 1
  end if
  x = x + 0.05d0
end do
end

      logical function IsInfinity_R8*4(value)
        implicit none
        real*8     value
        real*8     is_val_r
        integer*8  is_val_i

        equivalence (is_val_r, is_val_i)

        is_val_r = value

        IsInfinity_R8 = (IAND(is_val_i,z"7fffffffffffffff") == z"7ff0000000000000")

      return
      end

      logical function IsNan_R8*4(value)
        implicit none
        real*8     value
        real*8     is_val_r
        integer*8  is_val_i

        equivalence (is_val_r, is_val_i)

        is_val_r = value

        IsNan_R8 = ( (IAND(is_val_i,z"7ff0000000000000") == z"7ff0000000000000") .AND. (IAND(is_val_i, z"000fffffffffffff") /= 0) )

      return
      end

      logical function precision_range_R8*4(val_first, val_second, range)
        implicit none
        real*8      val_first, val_second
        real*8      range      ! Default one for the non configurable version was /.00000000000001D0/
        real*8      Absolute_Tolerance /z"0010000000000000"/   ! Minimum positive DP FP normal number
        real*8      temp_sum_r
        integer*8   temp_sum_i
        equivalence (temp_sum_r, temp_sum_i)

        logical*4   IsInfinity_R8, IsNan_R8

        ! If either number is NaN, the result is FALSE
        if (IsNan_R8(val_first) .OR. IsNan_R8(val_second)) then
           precision_range_R8 = .FALSE.
           return
        end if

        ! If either number is Infinity - looking for exact comparison, since
        !   Inf is close only to itself
        if (IsInfinity_R8(val_first) .OR. IsInfinity_R8(val_second)) then
           precision_range_R8 =  (val_first == val_second)
           return
        end if

        ! Dealing with number close to 0 - using Absolute Tolerance
        if ( ABS(val_first - val_second) .LE. Absolute_Tolerance) then
           precision_range_R8 = .TRUE.
           return
        end if

        temp_sum_r = val_first + val_second
        if  (IAND(temp_sum_i,z"7ff0000000000000") == z"7ff0000000000000" ) then ! if the above sum overflows
           temp_sum_r = ((val_first*range) + (val_second*range))
        else
           temp_sum_r = temp_sum_r*range
        end if
        precision_range_R8 = ((2*ABS(val_first - val_second)) .LE. temp_sum_r)

      return
      end

