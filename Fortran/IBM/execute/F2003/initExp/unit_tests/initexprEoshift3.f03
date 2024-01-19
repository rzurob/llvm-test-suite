!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : EOSHIFT intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none
integer :: i, k, j
logical :: precision_x8, precision_x16, precision_x32
complex(4), parameter, dimension(48) :: c0=(/((i,2*i+1.0),i=-24,23)/)
complex(4), dimension(48) :: c1=eoshift(c0, 3)

complex(4), parameter, dimension(2,3) :: c2=reshape(c0, (/2,3/))
complex(4), dimension(2,3) :: c3res, c3=eoshift(c2, shift=(/1,-1,0/), dim=1)

complex(4), parameter, dimension(3,2,8) :: c4=reshape(c0, (/3,2,8/))
complex(4), dimension(3,2,8) :: &
 & c4a=eoshift(c4, shift=reshape((/-1,0,1,2,3,4/), (/3,2/)), dim=3)
complex(4), dimension(3,2,8) :: c4res

complex(8), dimension(131), parameter :: d0=(/((dble(i*0.5),dble(i*0.3)),i=1,131)/)
complex(8), dimension(131) :: d1res, d1=eoshift(d0, shift=-2, boundary=(-1.1_8,-0.7_8))

complex(16), dimension(17), parameter :: e0=(1.7Q3, -1.9Q5)
complex(16), dimension(17) :: e1res, e1=eoshift(e0, shift=16, boundary=(1.0_16, 1.0_16))

if (.not. all(c1 .eq. eoshift(c0,3))) error stop 1

c3res = eoshift(c2, shift=(/1,-1,0/), dim=1)
do i=1,2
  do j=1,3
    if (.not. precision_x8(c3(i,j), c3res(i,j))) then
      print *, i, j, c3(i,j), c3res(i,j)
      stop 2
    endif
  enddo
enddo

c4res = eoshift(c4, shift=reshape((/-1,0,1,2,3,4/), (/3,2/)), dim=3)
do i=1, 3
  do j=1, 2
    do k=1, 8
      if (.not. precision_x8(c4a(i,j,k), c4res(i,j,k))) then
        write(*,'(3i2,4z10.8)') i,j,k,c4a(i,j,k), c4res(i,j,k)
        stop 3
      endif
    enddo
  enddo
enddo

d1res = eoshift(d0, shift=-2, boundary=(-1.1_8,-0.7_8))
do i=1, 131
  if (.not. precision_x16(d1(i), d1res(i))) then
    stop 4
  endif
enddo

e1res = eoshift(e0, shift=16, boundary=(1.0_16, 1.0_16))
do i=1, 17
  if (.not. precision_x32(e1(i), e1res(i))) then
    stop 5
  endif
enddo

end
