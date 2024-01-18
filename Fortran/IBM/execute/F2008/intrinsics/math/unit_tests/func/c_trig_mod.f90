module c_trig
  implicit none

  interface c_acos
    subroutine c_fc_acos(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_acos(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_acos(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface c_acos

  interface c_asin
    subroutine c_fc_asin(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_asin(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_asin(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_atan
    subroutine c_fc_atan(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_atan(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_atan(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_acosh
    subroutine c_fc_acosh(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_acosh(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_acosh(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_asinh
    subroutine c_fc_asinh(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_asinh(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_asinh(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_atanh
    subroutine c_fc_atanh(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_atanh(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_atanh(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_cosh
    subroutine c_fc_cosh(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_cosh(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_cosh(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_sinh
    subroutine c_fc_sinh(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_sinh(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_sinh(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_tanh
    subroutine c_fc_tanh(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_tanh(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_tanh(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface c_tan
    subroutine c_fc_tan(x, res) bind(c)
      complex(4), intent(in) :: x
      complex(4), intent(out) :: res
    end subroutine

    subroutine c_dc_tan(x, res) bind(c)
      complex(8), intent(in) :: x
      complex(8), intent(out) :: res
    end subroutine

    subroutine c_ldc_tan(x, res) bind(c)
      complex(16), intent(in) :: x
      complex(16), intent(out) :: res
    end subroutine
  end interface

  interface are_equal
    logical(4) function precision_x8(a, b)
      complex(4) a, b
    end function

    logical(4) function precision_x16(a, b)
      complex(8) a, b
    end function

    logical(4) function precision_x32(a, b)
      complex(16) a, b
    end function
  end interface
end module
