!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: eofiostat05.f
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

open(11, access='stream', form='formatted', status='scratch', pad='yes')
write(11, '(A)') 'abc'
rewind 11

c = '12345'
! pad=yes, advance=yes, record complete
read(11, '(A5)', iostat=iostat) c
if (iostat /= -1) error stop 1
if (c /= 'abc  ') error stop 2

c = '12345'
! pad=yes, advance=no, record complete
read(11, '(A5)', iostat=iostat, pos=1, advance='no') c
if (iostat /= -1) error stop 3
if (c /= 'abc  ') error stop 4

close(11)



open(11, access='stream', form='formatted', status='scratch', pad='yes')
write(11, '(A)', advance='no') 'abc'
rewind 11

c = '12345'
! pad=yes, advance=yes, record incomplete
read(11, '(A5)', iostat=iostat) c
if (iostat /= -1) error stop 5
if (c /= 'abc  ') error stop 6

c = '12345'
! pad=yes, advance=no, record incomplete
read(11, '(A5)', iostat=iostat, pos=1, advance='no') c
if (iostat /= -1) error stop 7
if (c /= 'abc  ') error stop 8
close(11)



open(11, access='stream', form='formatted', status='scratch', pad='no')
write(11, '(A)') 'abc'
rewind 11

c = '12345'
! pad=no, advance=yes, record complete
read(11, '(A5)', iostat=iostat) c
if (iostat /= -1) error stop 9
if (c /= 'abc45') error stop 10

c = '12345'
! pad=no, advance=no, record complete
read(11, '(A5)', iostat=iostat, pos=1, advance='no') c
if (iostat /= -1) error stop 11
if (c /= 'abc45') error stop 12

close(11)


open(11, access='stream', form='formatted', status='scratch', pad='no')
write(11, '(A)', advance='no') 'abc'
rewind 11

c = '12345'
! pad=no, advance=yes, record incomplete
read(11, '(A5)', iostat=iostat) c
if (iostat /= -1) error stop 13
if (c /= 'abc45') error stop 14

c = '12345'
! pad=no, advance=no, record incomplete
read(11, '(A5)', iostat=iostat, pos=1, advance='no') c
if (iostat /= -1) error stop 15
if (c /= 'abc45') error stop 16

close(11)

end
