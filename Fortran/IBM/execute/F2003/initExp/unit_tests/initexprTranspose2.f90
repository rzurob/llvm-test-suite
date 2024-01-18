!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : TRANSPOSE intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

implicit none

logical(1), parameter :: T1=.true._1, F1=.false._1
logical(2), parameter :: T2=.true._2, F2=.false._2
logical(4), parameter :: T4=.true._4, F4=.false._4
logical(8), parameter :: T8=.true._8, F8=.false._8

logical(1), parameter, dimension(5,4) :: a=reshape( &
 & (/F1, F1, T1, F1, T1, F1, F1, T1, T1, T1, &
 &   F1, T1, F1, F1, F1, T1, F1, F1, T1, T1/), &
 & (/5,4/))

logical(2), parameter, dimension(11,2) :: B=reshape( &
 & (/T2, T2, T2, T2, F2, T2, T2, F2, T2, F2, &
     T2, T2, T2, T2, T2, F2, T2, F2, F2, F2, F2, F2/), &
 & (/11,2/))

logical(4), parameter, dimension(7,2) :: C=reshape( &
 & (/F4, T4, F4, T4, F4, F4, T4, F4, F4, T4, &
 &   F4, F4, F4, T4/), &
 & (/7,2/))

logical(8), parameter, dimension(13,1) :: D=reshape( &
 & (/F8, F8, T8, F8, T8, F8, F8, T8, T8, T8, &
 &   F8, T8, F8/), &
 & (/13,1/))

logical(1), dimension(4,5) :: res1=transpose(a)
logical(2), dimension(2,11) :: res2=transpose(B)
logical(4), dimension(2,7) :: res3=transpose(C)
logical(8), dimension(1,13) :: res4=transpose(D)

if (.not. all(res1 .eqv. transpose(a))) error stop 1
if (.not. all(res2 .eqv. transpose(B))) error stop 2
if (.not. all(res3 .eqv. transpose(C))) error stop 3
if (.not. all(res4 .eqv. transpose(D))) error stop 4

end
