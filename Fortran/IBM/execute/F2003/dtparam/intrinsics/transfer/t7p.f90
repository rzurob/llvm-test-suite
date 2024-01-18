!DEFFERRED LENGTH

module m1

type dtp (k,l)
	integer,kind::k
	integer,len :: l
	real(k) :: r(l)
end type

type dt (k,l)
	integer,kind::k
	integer,len :: l
	complex(k) :: c(l)
end type

end module m1

program a
use m1
type (dtp(4,:)), pointer :: dtp4(:), dtp42(:)
type (dtp(8,:)), pointer :: dtp8(:), dtp82(:)

type (dt(4,:)), pointer :: dt4(:), dt42(:)
type (dt(8,:)), pointer :: dt8(:), dt82(:)
real(kind=4) :: r4(8), r42(8)
real(kind=8) :: r8(8), r82(8)
complex(kind=4) :: c4(4), c42(4)
complex(kind=8) :: c8(4), c82(4)
integer k
equivalence(c4,r4)
equivalence(c8,r8)

allocate(dtp(4,2)::dtp4(4))
allocate(dtp(8,2)::dtp8(4))
allocate(dt(4,2)::dt4(2))
allocate(dt(8,2)::dt8(2))

r4 = (/1.1,2.2,3.3,4.4,5.5,6.6,7.7,8.8/)
r8 = (/1.1,2.2,3.3,4.4,5.5,6.6,7.7,8.8/)

allocate(dtp42(ubound(dtp4,1)), SOURCE = transfer(r4,dtp4))
allocate(dtp82(ubound(dtp8,1)), SOURCE = transfer(r8,dtp8))

!equality check
k = 0
do i=1,ubound(dtp42,1)
	do j=1,dtp42%l
		k = k + 1
		if(dtp42(i)%r(j) /= r4(k)) then
			print *, "error with dtp42(",i,")%r(",j,") = ",dtp42(i)%r(j),"r4(",k,") =",r4(k)
			STOP 1
		end if
	end do
end do
k = 0
do i=1,ubound(dtp82,1)
	do j=1,dtp82%l
		k = k + 1
		if(dtp82(i)%r(j) /= r8(k)) then
			print *, "error with dtp82(",i,")%r(",j,") = ",dtp82(i)%r(j),"r8(",k,") =",r8(k)
			STOP 2
		end if
	end do
end do

allocate(dt42(ubound(dt4,1)), SOURCE = transfer(c4,dt4))
allocate(dt82(ubound(dt8,1)), SOURCE = transfer(c8,dt8))

!equality check
k = 0
do i=1,ubound(dt42,1)
	do j=1,dt42%l
		k = k + 1
		if(dt42(i)%c(j) /= c4(k)) then
			print *, "error with dt42(",i,")%r(",j,") = ",dt42(i)%c(j),"c4(",k,") =",c4(k)
			STOP 3
		end if
	end do
end do
k = 0
do i=1,ubound(dt82,1)
	do j=1,dt82%l
		k = k + 1
		if(dt82(i)%c(j) /= c8(k)) then
			print *, "error with dt82(",i,")%r(",j,") = ",dt82(i)%c(j),"c8(",k,") =",c8(k)
			STOP 4
		end if
	end do
end do

dt4 = transfer(dtp42,dt42,2)
dt8 = transfer(dtp82,dt82,2)
!equality check
!checks each component of real dtp with corresponding component of complex dtp
k=1
do i=1,ubound(dtp42,1)
	do j=1,dtp42%l
		if (mod(j,2) == 0) then
			if(imag(dt4(int(i/3)+1)%c(k)) /= dtp42(i)%r(j)) then
				print *, "error with dt4(",int(i/3)+1,")%r(",k,") = ",cmplx(dt4(int(i/3)+1)%c(k)),"dtp42(",i,")%r(",j,") = ",dtp42(i)%r(j)
				STOP 5
			end if
			if (k == 1) then
				k = 2
			else
				k = 1
			end if
		else
			if(real(dt4(int(i/3)+1)%c(k)) /= dtp42(i)%r(j)) then
				print *, "error with dt4(",int(i/3)+1,")%r(",k,") = ",real(dt4(int(i/3)+1)%c(k)),"dtp4(",i,")%r(",j,") = ",dtp42(i)%r(j)
				STOP 6
			end if
		end if
	end do
end do
k=1
do i=1,ubound(dtp8,1)
	do j=1,dtp8%l
		if (mod(j,2) == 0) then
			if(imag(dt8(int(i/3)+1)%c(k)) /= dtp82(i)%r(j)) then
				print *, "error with dt8(",int(i/3)+1,")%r(",k,") = ",dt8(int(i/3)+1)%c(k),"dtp8(",i,")%r(",j,") = ",dtp82(i)%r(j)
				STOP 7
			end if
			if (k == 1) then
				k = 2
			else
				k = 1
			end if
		else
			if(real(dt8(int(i/3)+1)%c(k)) /= dtp82(i)%r(j)) then
				print *, "error with dt8(",int(i/3)+1,")%r(",k,") = ",dt8(int(i/3)+1)%c(k),"dtp8(",i,")%r(",j,") = ",dtp82(i)%r(j)
				STOP 8
			end if
		end if
	end do
end do

r42 = transfer(dtp42,r4)
r82 = transfer(dtp82,r8)
c42 = transfer(dt42,c4)
c82 = transfer(dt82,c8)

do i=1,4
if (r4(i) /= r42(i)) then
	print *, "error with r42 = transfer(dtp42,r4), r42 =", r42,"r4 =",r4
	STOP 9
endif
if (r82(i) /= r8(i)) then
	print *, "error with r82 = transfer(dtp82,r8), r82 =", r82,"r8 =",r8
	STOP 10
endif
end do
do i=1,2
if (c4(i) /= c42(i)) then
	print *, "error with r42 = transfer(dtp42,r4), r42 =", r42,"r4 =",r4
	STOP 11
endif
if (c82(i) /= c8(i)) then
	print *, "error with r82 = transfer(dtp82,r8), r82 =", r82,"r8 =",r8
	STOP 12
endif
end do
end program a