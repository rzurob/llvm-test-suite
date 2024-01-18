! ERFC, which was an IBM extension, is now part of F2008.
! Check that we issue langlvl messages for DERFC and QERFC, but not
! ERFC with real(8) arguments.
implicit none
real(4) e, eres
real(8) d, dres, dres2
real(16) q, qres, qres2

interface
  logical function precision_r4(a, b)
    real(4), intent(in) :: a, b
  end function
  logical function precision_r8(a, b)
    real(8), intent(in) :: a, b
  end function
  logical function precision_r16(a, b)
    real(16), intent(in) :: a, b
  end function
end interface

e = 1.0e0
eres = erfc(e)
if (.not. precision_r4(eres, 0.1572992e0)) error stop 1

d = 1.0d0
dres = erfc(d)
if (.not. precision_r8(dres, 0.157299207050285d0)) error stop 2

dres2 = derfc(d)
if (.not. precision_r8(dres2, dres)) error stop 3

q = 1.0q0
qres = erfc(q)
if (.not. precision_r16(qres, 0.1572992070502851306587793649174q0)) error stop 4

qres2 = qerfc(q)
if (.not. precision_r16(qres2, qres)) error stop 5

end
