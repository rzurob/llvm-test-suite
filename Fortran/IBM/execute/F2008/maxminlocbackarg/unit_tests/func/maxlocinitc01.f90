! Call to maxloc in initialization expression with rank 1 character array and
! 1. no BACK argument
! 2. BACK = .true.
! 3. BACK = .false.
! 4. MASK is present and BACK = .true.
! 5. MASK is present and BACK = .false.
! 6. MASK and KIND are present and BACK = .true.
! 7. MASK and KIND are present and BACK = .false.
! 8. KIND is present and BACK = .true.
! 9. KIND is present and BACK = .false.

implicit none

integer :: j1(1) = maxloc((/'a', 'c', 'c', 'b'/))

integer :: j2(1) = maxloc((/'a', 'c', 'c', 'b'/), back = .true.)

integer :: j3(1) = maxloc((/'aa', 'cc', 'cc', 'bb'/), back = .false.)

integer :: j4(1) = maxloc((/'a', 'b', 'c', 'b'/), &
                          (/.true., .true., .false., .true./), back = .true.)

integer :: j5(1) = maxloc((/'a', 'b', 'c', 'b'/), &
                          mask = (/.true., .true., .false., .true./), back = .false.)

integer :: j6(1) = maxloc((/'aaaa', 'bbbb', 'cccc', 'bbbb'/), &
                          (/.true., .true., .false., .true./), 4, .true.)

integer :: j7(1) = maxloc((/'a', 'b', 'c', 'b'/), &
                          (/.true., .true., .false., .true./), kind = 4, back = .false.)

integer :: j8(1) = maxloc((/'a', 'c', 'b', 'c'/), kind = 4, back = .true.)

integer :: j9(1) = maxloc((/'a', 'c', 'b', 'c'/), kind = 4, back = .false.)

if (j1(1) .ne. 2) ERROR STOP 1
if (j2(1) .ne. 3) ERROR STOP 2
if (j3(1) .ne. 2) ERROR STOP 3
if (j4(1) .ne. 4) ERROR STOP 4
if (j5(1) .ne. 2) ERROR STOP 5
if (j6(1) .ne. 4) ERROR STOP 6
if (j7(1) .ne. 2) ERROR STOP 7
if (j8(1) .ne. 4) ERROR STOP 8
if (j9(1) .ne. 2) ERROR STOP 9

end
