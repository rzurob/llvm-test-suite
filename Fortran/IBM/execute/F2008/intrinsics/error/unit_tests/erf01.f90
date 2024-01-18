! ERF, which was an IBM extension, is now part of F2008.
! Check that we issue langlvl messages for DERF and QERF, but not
! ERF with real(8) arguments.
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
eres = erf(e)
if (.not. precision_r4(eres, 0.8427008e0)) error stop 1

d = 1.0d0
dres = erf(d)
if (.not. precision_r8(dres, 0.842700792949715d0)) error stop 2

dres2 = derf(d)
if (.not. precision_r8(dres2, dres)) error stop 3

q = 1.0q0
qres = erf(q)
if (.not. precision_r16(qres, 0.8427007929497148693412206350826q0)) error stop 4

qres2 = qerf(q)
if (.not. precision_r16(qres2, qres)) error stop 5

end
