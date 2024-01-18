!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : signed zeros as args to ATAN2, LOG and SQRT intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Passing signed zeros as arguments to
!*                               ATAN2,LOG and SQRT intrinsics to make sure
!*															 the compiler conforms to Fortran 2003 standard
!*
!234567890123456789012345678901234567890123456789012345678901234567890
real ::a,b,c,d,e
complex :: f,g,h,i,j,k
real(8) :: l,m,n,o,p
complex(8) :: q,r,s,t,u,v
logical precision_r4, precision_r8, precision_x8, precision_x16

!real(4)
a=-1.0
b=-0.0
c=0.0

!real(8)
l=-1.0
m=-0.0
n=0.0

!complex(4)
f=(-1.0,-0.0)
g=(-1.0,0.0)

!complex(8)
q=(-1.0,-0.0)
r=(-1.0,0.0)

!passing negative zero to atan2, log, sqrt
h=sqrt(f)
d=atan2(b,a)
i=log(f)
s=sqrt(q)
o=atan2(m,l)
t=log(q)

!passing positive zero to atan2, log, sqrt
j=sqrt(g)
e=atan2(c,a)
k=log(g)
u=sqrt(r)
p=atan2(n,l)
v=log(r)


if(.not.precision_x8(h,(-0.0000000000E+00,1.000000000))) error stop 1
if(.not.precision_r4(d,3.141592741)) error stop 2
if(.not.precision_x8(i,(0.0000000000E+00,3.141592741))) error stop 3
if(.not.precision_x16(s,(-0.000000000000000000d0,1.00000000000000000d0))) error stop 4
if(.not.precision_r8(o,3.14159265358979312d0)) error stop 5
if(.not.precision_x16(t,(0.000000000000000000d0,3.14159265358979312d0))) error stop 6
if(.not.precision_x8(j,(0.0000000000E+00,1.000000000))) error stop 7
if(.not.precision_r4(e,3.141592741)) error stop 8
if(.not.precision_x8(k,(0.0000000000E+00,3.141592741))) error stop 9
if(.not.precision_x16(u,(0.000000000000000000d0,1.00000000000000000d0))) error stop 10
if(.not.precision_r8(p,3.14159265358979312d0)) error stop 11
if(.not.precision_x16(v,(0.000000000000000000d0,3.14159265358979312d0))) error stop 12

end program

