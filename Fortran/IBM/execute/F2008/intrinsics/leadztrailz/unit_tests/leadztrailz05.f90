! F2008 leadz and trailz intrinsics with INTSIZE and typeless arguments
implicit none
call test_intsize2
call test_intsize4
call test_intsize8
end

@PROCESS INTSIZE(2)
subroutine test_intsize2
  implicit none
  integer i, leadz_result, trailz_result
100 format (" ",b64.16," ",i2," ",i2)

  i = z'ff'
  print 100, i, leadz(i), trailz(i)
  i = z'f0'
  print 100, i, leadz(i), trailz(i)
  i = z'0f'
  print 100, i, leadz(i), trailz(i)
  i = z'00'
  print 100, i, leadz(i), trailz(i)
  i = z'04'
  print 100, i, leadz(i), trailz(i)
  i = z'40'
  print 100, i, leadz(i), trailz(i)
  i = z'80'
  print 100, i, leadz(i), trailz(i)
  i = z'01'
  print 100, i, leadz(i), trailz(i)

  ! typeless arguments
  print 100, z'00', leadz(z'00'), trailz(z'00')
  print 100, z'f0', leadz(z'f0'), trailz(z'f0')
  print 100, z'0f', leadz(z'0f'), trailz(z'0f')
  print 100, z'ff', leadz(z'ff'), trailz(z'ff')
end subroutine

@PROCESS INTSIZE(4)
subroutine test_intsize4
  implicit none
  integer i, leadz_result, trailz_result
100 format (" ",b64.32," ",i2," ",i2)

  i = z'ff'
  print 100, i, leadz(i), trailz(i)
  i = z'f0'
  print 100, i, leadz(i), trailz(i)
  i = z'0f'
  print 100, i, leadz(i), trailz(i)
  i = z'00'
  print 100, i, leadz(i), trailz(i)
  i = z'04'
  print 100, i, leadz(i), trailz(i)
  i = z'40'
  print 100, i, leadz(i), trailz(i)
  i = z'80'
  print 100, i, leadz(i), trailz(i)
  i = z'01'
  print 100, i, leadz(i), trailz(i)

  ! typeless arguments
  print 100, z'00', leadz(z'00'), trailz(z'00')
  print 100, z'f0', leadz(z'f0'), trailz(z'f0')
  print 100, z'0f', leadz(z'0f'), trailz(z'0f')
  print 100, z'ff', leadz(z'ff'), trailz(z'ff')
end subroutine

@PROCESS INTSIZE(8)
subroutine test_intsize8
  implicit none
  integer i, leadz_result, trailz_result
100 format (" ",b64.64," ",i2," ",i2)

  i = z'ff'
  print 100, i, leadz(i), trailz(i)
  i = z'f0'
  print 100, i, leadz(i), trailz(i)
  i = z'0f'
  print 100, i, leadz(i), trailz(i)
  i = z'00'
  print 100, i, leadz(i), trailz(i)
  i = z'04'
  print 100, i, leadz(i), trailz(i)
  i = z'40'
  print 100, i, leadz(i), trailz(i)
  i = z'80'
  print 100, i, leadz(i), trailz(i)
  i = z'01'
  print 100, i, leadz(i), trailz(i)

  ! typeless arguments
  print 100, z'00', leadz(z'00'), trailz(z'00')
  print 100, z'f0', leadz(z'f0'), trailz(z'f0')
  print 100, z'0f', leadz(z'0f'), trailz(z'0f')
  print 100, z'ff', leadz(z'ff'), trailz(z'ff')
end subroutine
