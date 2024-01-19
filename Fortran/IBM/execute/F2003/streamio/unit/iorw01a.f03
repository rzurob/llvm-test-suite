! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Mar. 1, 2003
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN, READ and WRITE
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access,
!*                              : read it at specific position which
!*                              : is pointed by POS specifier, and
!*                              : rewrite it.
!234567890123456789012345678901234567890123456789012345678901234567890
program iorw01a
  character(4) c
  integer(4) :: I = 5, J = 2, K

  open(1, file='mystream', access='stream', form='formatted', &
       action='readwrite')

  ! Attempt to do advancing character input starting
  ! at file position 2.

  read(1, FMT='(A4)', ADVANCE='YES', POS=J) c
  print *, c

  write(1, FMT='(I4)', ADVANCE='YES', POS=J) I
  inquire(1, pos=K)
  print *, K
  close(1)

end program iorw01a

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1   5\nc\n"
  ! after program is done.
