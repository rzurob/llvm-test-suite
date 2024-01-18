!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : PACK intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

implicit none
integer :: i

logical, parameter :: T=.true., F=.false.
logical(8), parameter :: T8=.true._8, F8=.false._8

logical(1), parameter :: z0(200)=(/(.true.,i=1,200)/)
logical(1), parameter :: zarray(2,5,2)=reshape(z0, (/2,5,2/))
logical(1) :: z(100)=pack(zarray, mask=zarray, vector=(/(.false._1,i=1,100)/))

logical(2), parameter :: y0(100)=(/(.false.,i=1,100)/)
logical(2), parameter :: yarr(5,4,5)=reshape(y0, (/5,4,5/))
logical(2) :: y(100)=pack(yarr, mask=.true.)

logical(4), parameter, dimension(3,3) :: i0=reshape((/F,T,F,F,F,F,F,F,T/),(/3,3/))
logical(4) :: i4(7)=pack(i0, mask=i0 .eqv. F)
logical(4) :: i4a(6)=pack(i0, mask=i0 .eqv. T, vector=(/T,F,T,F,T,F/))

logical(8), parameter, dimension(2,3) :: &
  &            a0=reshape((/F8,F8,F8,T8,T8,T8/),(/2,3/))
logical(8) :: a4(3)=pack(a0, mask=a0)
logical(8) :: a4a(7)=pack(a0, mask=.not. a0, &
  &            vector=(/T8,F8,F8,T8,T8,T8,F8/))

if (.not. all(z .eqv. pack(zarray, mask=zarray, vector=(/(.false._1,i=1,100)/)))) stop 1

if (.not. all(y .eqv. pack(yarr, mask=.true.))) stop 2

if (.not. all(i4 .eqv. pack(i0, mask=i0 .eqv. F))) stop 3
if (.not. all(i4a .eqv. pack(i0, mask=i0 .eqv. T, vector=(/T,F,T,F,T,F/)))) stop 4

if (.not. all(a4 .eqv. pack(a0, mask=a0))) stop 5
if (.not. all(a4a .eqv. pack(a0, mask=.not. a0, &
  & vector=(/T8,F8,F8,T8,T8,T8,F8/)))) stop 6

end
