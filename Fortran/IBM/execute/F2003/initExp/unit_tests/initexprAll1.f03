!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ALL intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

logical(1) :: e1T=all((/.true._1/)), e1F=all((/.false._1/))
logical(2) :: e2T=all((/.true._2/)), e2F=all((/.false._2/))
logical(4) :: e4T=all((/.true._4/)), e4F=all((/.false._4/))
logical(8) :: e8T=all((/.true._8/)), e8F=all((/.false._8/))

logical :: eT=all((/.true., .false., .true./)), &
   & eF=all((/.false., .true., .false./))

if (e1T .neqv. all((/.true._1/))) error stop 1
if (e1T .neqv. all((/.true._1/))) error stop 2
if (e2T .neqv. all((/.true._2/))) error stop 3
if (e4T .neqv. all((/.true._4/))) error stop 4
if (e8T .neqv. all((/.true._8/))) error stop 5
if (e1F .neqv. all((/.false._1/))) error stop 6
if (e2F .neqv. all((/.false._2/))) error stop 7
if (e4F .neqv. all((/.false._4/))) error stop 8
if (e8F .neqv. all((/.false._8/))) error stop 9

if (eT) error stop 10
if (eF) error stop 11
end