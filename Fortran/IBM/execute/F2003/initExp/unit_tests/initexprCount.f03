!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : COUNT intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

implicit none

integer(1) :: e1T=count((/.true._1/)), e1F=count((/.false._1/))
integer(2) :: e2T=count((/.true._2/)), e2F=count((/.false._2/))
integer(4) :: e4T=count((/.true._4/)), e4F=count((/.false._4/))
integer(8) :: e8T=count((/.true._8/)), e8F=count((/.false._8/))

logical, parameter, dimension(2,3) :: &
 & A=reshape((/.true., .false., .false., .true., &
 &             .false., .true./), (/2,3/)), &
 & B=reshape((/.false., .true., .false., .true., &
 &             .true., .true./), (/2,3/))
integer :: res1=count(A .eqv. B), res2(3)=count(A .eqv. B, dim=1), &
 &         res3(2)=count(A .eqv. B, dim=2)

if (e1T .ne. count((/.true._1/))) then
  print *, e1T, count((/.true._1/))
  stop 1
endif
if (e2T .ne. count((/.true._2/))) error stop 2
if (e4T .ne. count((/.true._4/))) error stop 3
if (e8T .ne. count((/.true._8/))) error stop 4
if (e1F .ne. count((/.false._1/))) error stop 5
if (e2F .ne. count((/.false._2/))) error stop 6
if (e4F .ne. count((/.false._4/))) error stop 7
if (e8F .ne. count((/.false._8/))) error stop 8
end
