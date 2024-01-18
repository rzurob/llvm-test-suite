! This is a copy of /xlftest/src/zzrc/check_prec.f, modified here to declare the
! procedures as pure for use by the pure procedures in this bucket.
! It includes only the scalar versions of the precision_* routines:
! 
module pcheck_prec
    interface prec
        module procedure precision_R4
        module procedure precision_R8
        module procedure precision_R6
        module procedure precision_x8
        module procedure precision_x6
        module procedure precision_x3
    end interface
    contains

!-------------------------------------------------------
!
!   value -> The computed number.
!     exp -> The expected number.
!
!   When a new compiler is produced uncomment the lines
!   for real*16 complex*32.
!
!--------------------------------------------------------
!                      Real*4
!--------------------------------------------------------
!===========================================================================
!RBM: This is the scalar version of this routine
!RBM: It is unchanged from the original in /fort1/v2r1/zz
!===========================================================================
      logical pure function precision_r4*4(value,exp)
      use, intrinsic :: ISO_C_BINDING
      real(C_FLOAT), intent(in) :: value,exp
      real(C_FLOAT) range,high_a,low_a,temp

      range = 0.00001

      temp = exp*range
      high_a = temp + exp
      low_a = exp - temp

      if(exp < 0.0E0) then
          precision_R4 = ((value.ge.high_a).and.(value.le.low_a))
      else
          precision_R4 = ((value.le.high_a).and.(value.ge.low_a))
      end if
      return
      end function precision_R4

!-------------------------------------------------------
!
!   value -> The computed number.
!     exp -> The expected number.
!
!   When a new compiler is produced uncomment the lines
!   for real*16 complex*32.
!
!--------------------------------------------------------
!                      Real*8
!--------------------------------------------------------
!===========================================================================
!RBM: This is the scalar version of this routine
!RBM: It is unchanged from the original in /fort1/v2r1/zz
!===========================================================================
      logical pure function precision_R8*4(value,exp)
      real*8, intent(in) :: value,exp
      real*8 range,high_a,low_a,temp

      range = 0.00000000000001D0

      temp = exp*range
      high_a = temp + exp
      low_a = exp - temp

      if(exp < 0.0D0) then
          precision_R8 = ((value.ge.high_a).and.(value.le.low_a))
      else
          precision_R8 = ((value.le.high_a).and.(value.ge.low_a))
      end if
      return
      end function precision_R8

!-------------------------------------------------------
!
!   value -> The computed number.
!     exp -> The expected number.
!
!   When a new compiler is produced uncomment the lines
!   for real*16 complex*32.
!
!--------------------------------------------------------
!                      Real*16
!--------------------------------------------------------
!===========================================================================
!RBM: This is the scalar version of this routine
!RBM: It is slightly modified from the original in /fort1/v2r1/zz
!RBM: (I got rid of the entry stuff, since it is not needed when everything
!RBM:  is being handled by a generic interface anyway)
!===========================================================================
      logical pure function precision_R6*4(value,exp)
      real*16, intent(in) :: value,exp
      real*16 range,high_a,low_a,temp

      range = 0.0000000000000000000000000000001Q0

      temp = exp*range
      high_a = temp + exp
      low_a = exp - temp

      if(exp < 0.0D0) then
          precision_R6 = ((value.ge.high_a).and.(value.le.low_a))
      else
          precision_R6 = ((value.le.high_a).and.(value.ge.low_a))
      end if
      return
      end function precision_R6

!-------------------------------------------------------
!
!   value -> The computed number.
!     exp -> The expected number.
!
!   When a new compiler is produced uncomment the lines
!   for real*16 complex*32.
!
!--------------------------------------------------------
!                   Complex*8
!--------------------------------------------------------
!===========================================================================
!RBM: This is the scalar version of this routine
!RBM: It is unchanged from the original in /fort1/v2r1/zz
!===========================================================================
      logical pure function precision_x8*4(value,exp)
      complex*8, intent(in) :: value,exp
      real*4 imag,reel,range,high_a,low_a,temp1
      real*4 temp2,high_b,low_b,expreel,expimag
      logical*4 t_log1, t_log2

      range = 0.00001

      reel = value              ! reel portion of the computed complex
      imag = aimag(value)       ! imaginary portion of the computed complex

      expreel = exp             ! reel portion of the expected complex
      expimag = aimag(exp)      ! imaginary portion of the expected complex

      temp1 = expreel*range
      high_a = temp1 + expreel
      low_a = expreel - temp1

      temp2 = expimag*range
      high_b = temp2+expimag
      low_b = expimag - temp2

      if(expreel < 0.0E0) then
          t_log1 = ((reel.ge.high_a).and.(reel.le.low_a))
      else
          t_log1 = ((reel.le.high_a).and.(reel.ge.low_a))
      end if
      if(expimag < 0.0E0) then
          t_log2 = ((imag.ge.high_b).and.(imag.le.low_b))
      else
          t_log2 = ((imag.le.high_b).and.(imag.ge.low_b))
      end if
      precision_x8 = t_log1 .and. t_log2
      return
      end function precision_x8

!-------------------------------------------------------
!
!   value -> The computed number.
!     exp -> The expected number.
!
!   When a new compiler is produced uncomment the lines
!   for real*16 complex*32.
!
!--------------------------------------------------------
!                   Complex*16
!--------------------------------------------------------
!===========================================================================
!RBM: This is the scalar version of this routine
!RBM: It is slightly modified from the original in /fort1/v2r1/zz
!RBM: (I got rid of the entry stuff, since it is not needed when everything
!RBM:  is being handled by a generic interface anyway)
!===========================================================================
      logical pure function precision_x6*4(value,exp)
      complex*16, intent(in) :: value,exp
      real*8 imag,reel,range,high_a,low_a,temp1
      real*8 temp2,high_b,low_b,expreel,expimag
      logical*4 t_log1, t_log2, precision_x16

      range = 0.00000000000001D0

      reel = value              ! reel portion of the computed complex
      imag = dimag(value)       ! imaginary portion of the computed complex

      expreel = exp             ! reel portion of the expected complex
      expimag = dimag(exp)      ! imaginary portion of the expected complex

      temp1 = expreel*range
      high_a = temp1 + expreel
      low_a = expreel - temp1

      temp2 = expimag*range
      high_b = temp2+expimag
      low_b = expimag - temp2

      if(expreel < 0.0D0) then
          t_log1 = ((reel.ge.high_a).and.(reel.le.low_a))
      else
          t_log1 = ((reel.le.high_a).and.(reel.ge.low_a))
      end if
      if(expimag < 0.0D0) then
          t_log2 = ((imag.ge.high_b).and.(imag.le.low_b))
      else
          t_log2 = ((imag.le.high_b).and.(imag.ge.low_b))
      end if
      precision_x6 = t_log1 .and. t_log2
      return
      end function precision_x6

!-------------------------------------------------------
!
!   value -> The computed number.
!     exp -> The expected number.
!
!   When a new compiler is produced uncomment the lines
!   for real*16 complex*32.
!
!--------------------------------------------------------
!                   Complex*32
!--------------------------------------------------------
!===========================================================================
!RBM: This is the scalar version of this routine
!RBM: It is slightly modified from the original in /fort1/v2r1/zz
!RBM: (I got rid of the entry stuff, since it is not needed when everything
!RBM:  is being handled by a generic interface anyway)
!===========================================================================
      logical pure function precision_x3*4(value,exp)
      complex*32, intent(in) :: value,exp
      real*16 imag,reel,high_a,low_a,temp1
      real*16 range,temp2,high_b,low_b,expreel,expimag
      logical*4 t_log1, t_log2, precision_x32

      range = 0.0000000000000000000000000000001Q0

      reel = value              ! reel portion of the computed complex
      imag = qimag(value)       ! imaginary portion of the computed complex

      expreel = exp             ! reel portion of the expected complex
      expimag = qimag(exp)      ! imaginary portion of the expected complex

      temp1 = expreel*range
      high_a = temp1 + expreel
      low_a = expreel - temp1

      temp2 = expimag*range
      high_b = temp2+expimag
      low_b = expimag - temp2


      if(expreel < 0.0D0) then
          t_log1 = ((reel.ge.high_a).and.(reel.le.low_a))
      else
          t_log1 = ((reel.le.high_a).and.(reel.ge.low_a))
      end if
      if(expimag < 0.0D0) then
          t_log2 = ((imag.ge.high_b).and.(imag.le.low_b))
      else
          t_log2 = ((imag.le.high_b).and.(imag.ge.low_b))
      end if
      precision_x3 = t_log1 .and. t_log2
      return
      end function precision_x3

end module pcheck_prec
