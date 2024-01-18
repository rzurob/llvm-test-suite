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
!*                              : rewrite it. POS= is defined in a
!*                              : module.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer(4) :: I = 5, J = 2, K
end module

program iorw08

  use mod
  character(4) c

  open(1, file='mystream', access='stream', form='formatted', &
       action='readwrite')

  ! Attempt to do non-advancing character input starting
  ! at file position 2.

  read(1, FMT='(A4)', ADVANCE='NO', EOR=100, POS=J) c
  print *, c

  ! Overwrite positions 2-5 (inclusive) with the value
  ! of the 4-byte integer variable I.
  write(1, FMT='(I4)', ADVANCE='NO', POS=J) I
  inquire(1, pos=K)
  print *, K
  close(1)

100   PRINT *, 'Reached the end of the unit.'
end program iorw08

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1   5bc\n" after the
  ! program is done.
