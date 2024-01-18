!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxsigndzerointr001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : new runtime routines for ATAN2, LOG and SQRT intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=signdzerointr
!*
!*  DESCRIPTION                : Passing generic values as arguments to
!*															 ATAN2,LOG and SQRT intrinsics to make sure
!*															 the compiler conforms to Fortran 2003 standard
!*
!234567890123456789012345678901234567890123456789012345678901234567890
real ::a,b,c,d
complex :: f,g,h,i
real(8) :: j,k,l,m
complex(8) :: n,o,p,q
logical precision_r4, precision_r8, precision_x8, precision_x16

!real(4)
a=-1.0
b=1.0


!real(8)
j=-1.0
k=1.0

!complex(4)
f=(1.5,2.5)
g=(1.0,2.0)

!complex(8)
n=(1.5_d0,2.5_d0)
o=(1.0,2.0)


c=atan2(a,b)
d=atan2(b,b)

h=sqrt(f)
i=log(g)

l=atan2(j,k)
m=atan2(k,k)

q=sqrt(n)
p=log(o)



if(.not.precision_r4(c,-0.7853981853)) error stop 1
if(.not.precision_r4(d,0.7853981853)) error stop 2
if(.not.precision_x8(h,(1.485845923,0.8412716389))) error stop 3
if(.not.precision_x8(i,(0.8047189713,1.107148767))) error stop 4
if(.not.precision_r8(l,-0.785398163397448279d0)) error stop 5
if(.not.precision_r8(m,0.785398163397448279d0)) error stop 6
if(.not.precision_x16(p,(0.804718956217050252d0,1.10714871779409041d0))) error stop 7
if(.not.precision_x16(q,(1.48584587818229807d0,0.841271640857651493d0))) error stop 8


end program


