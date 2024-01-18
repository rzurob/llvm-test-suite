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
!*                              : rewrite it. POS is defined as constant.
!*                              : It is write first and read second.
!234567890123456789012345678901234567890123456789012345678901234567890
program iorw05
  character(4) c
  integer K

  open(1, file='mystream', access='stream', form='formatted', &
       action='readwrite')

  ! Overwrite positions 2-5 (inclusive) with the value
  ! of the 4-byte integer variable 5.
  write(1, FMT='(I4)', ADVANCE='NO', POS=2) 5
  inquire(1, pos=K)
  print *, K

  ! Attempt to do non-advancing character input starting
  ! at file position 2.

  read(1, FMT='(A4)', ADVANCE='NO', EOR=100, POS=2) c
  print *, c
  close(1)

100   PRINT *, 'Reached the end of the unit.'
end program iorw05

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1   5bc\n" after the
  ! program is done.
