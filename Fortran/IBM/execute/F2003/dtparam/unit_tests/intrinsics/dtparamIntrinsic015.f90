!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Intrinsic  with Derived Type Parameter
!*
!* PROGRAMMER                 : James Ren
!* DATE                       : April 19, 2007
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : real, imag and kind intrinsics
!*
!* ===================================================================


module mod
integer, parameter :: KP = 4
type base(baseKind)
   integer, kind  :: baseKind
   real(kind=baseKind) :: r1, r2
end type

type (base(KP)), allocatable :: baset

contains

function cmp(arg)
   type (base(KP)) :: arg
   complex(kind=KP) :: cmp
   cmp = (arg%r1, arg%r2)
end function
end module

use mod
implicit none
complex(KP) :: cplx
type(base(KP)) :: t = base(KP)(r1=1.0_KP, r2=2.0_KP)
allocate(baset, source = t)
cplx = cmp(baset)

if (real(cplx) /= 1.0_KP) error stop 1
if (imag(cplx) /= 2.0_KP) error stop 2
if (kind(real(cplx)) /= KP) error stop 3
if (kind(imag(cplx)) /= KP) error stop 4

end
