!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: eofiostat04.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : EOF testing
!*
!234567890123456789012345678901234567890123456789012345678901234567890
character*5 c
integer ipos, iostat


! File consists of one incomplete record.  No padding.
open(11, access='stream', form='formatted', status='scratch', pad='no')
write(11, '(A)', advance='no') "abc"
rewind 11

read(11, '(A5)', iostat=iostat) c
if (iostat /= -1) error stop 1

read(11, '(A5)', iostat=iostat, pos=1, advance='no') c
if (iostat /= -1) error stop 1

close(11)


! File consists of one complete record.  No padding.
open(11, access='stream', form='formatted', status='scratch', pad='no')
write(11, '(A)') "abc"
rewind 11

read(11, '(A5)', iostat=iostat) c
if (iostat /= -1) error stop 1 ! EOF

read(11, '(A5)', iostat=iostat, pos=1, advance='no') c
if (iostat /= -1) error stop 2 ! EOF

close(11)


! File consists of two complete records.  No padding.
! Read past the end of the first record.
open(11, access='stream', form='formatted', status='scratch', pad='no')
write(11, '(A)') "abc"
write(11, '(A)') "de"
rewind 11

read(11, '(A5)', iostat=iostat) c
if (iostat /= 4) error stop 3 ! EOR on advancing IO

read(11, '(A5)', iostat=iostat, pos=1, advance='no') c
if (iostat /= -4) error stop 4! EOR on non-advancing IO

close(11)


! File consists of two complete records.  No padding.
! read past the end of the second record / the end of file.
open(11, access='stream', form='formatted', status='scratch', pad='no')
write(11, '(A)') "abc"
write(11, '(A)') "de"
rewind 11

read(11, '(A5)', iostat=iostat, pos=20) c
if (iostat /= -1) error stop 5 ! EOF

read(11, '(A5)', iostat=iostat, pos=20, advance='no') c
if (iostat /= -1) error stop 6 ! EOF

! After an EOF condition, the file is positioned at its terminal point
inquire(11, pos=ipos)
if (ipos /= 8) error stop 7

close(11)


end
