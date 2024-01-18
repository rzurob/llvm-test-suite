! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iounit.presh ioopin01
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Mar. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN and INQUIRE
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access
!*                              : method with different format and
!*                              : specifiers, and inquire it.
!234567890123456789012345678901234567890123456789012345678901234567890
program ioopin01

character(3) :: c = ''
integer x, y, z

open(unit=1, file='newStreamFile', access='stream')
inquire(unit=1, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(1)

open(2, access='stream', form='formatted', status='scratch')
inquire(2, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(2)

open(unit=3, file='existStreamFile', access='stream', status='old', &
&	action='read')
inquire(unit=3, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(3)

open(4, file='newStreamFile', access='stream', form='formatted', &
&	pad='no', action='write')
inquire(4, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(4)

open(unit=7, file='newStreamFile', access='stream', form='formatted', &
&	pad='no', action='readwrite', iostat=z )
inquire(unit=7, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(7)

open(8, file='newStreamFile', access='stream', form='formatted', &
&	pad='no', action='readwrite', iostat=z, err=100)
inquire(8, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(8)

open(unit=9, file='newStreamFile', access='stream', form='formatted', &
&	pad='no', action='readwrite', iostat=z, err=100, blank='zero')
inquire(unit=9, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(9)

open(10, file='newStreamFile', access='stream', form='formatted', pad='no', &
&	action='readwrite', iostat=z, err=100, blank='zero', delim='quote')
inquire(10, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(10)

open(unit=11, file='newStreamFile', access='stream', form='unformatted', &
&	action='readwrite', iostat=z, err=100, asynch='yes')
inquire(unit=11, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(11)

open(12, file='existStreamFile', access='stream', form='formatted', &
&	pad='no', action='read', iostat=z, err=100, blank='zero', &
&	delim='quote', position='append')
inquire(12, stream=c, pos=x, size=y)
print *, c
print *, x
print *, y
close(12)

100 PRINT *, 'end of unit reached'
end program ioopin01
