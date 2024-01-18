! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Oct. 7, 2002
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN, READ/WRITE and INQUIRE
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access,
!*                              : read/write it with POS specifier,
!*                              : and inquire it with POS, SIZE and STREAM
!*                              : specifiers, it will be flagged with langage
!*				: level options.
!234567890123456789012345678901234567890123456789012345678901234567890
program iolang01

character(8) :: c
integer x, y

open(unit=1, file='newStreamFile', access='stream')
read(1, POS=1)
write(1, POS=1)
inquire(unit=1, stream=c, pos=x, size=y)
close(1)

end program iolang01
