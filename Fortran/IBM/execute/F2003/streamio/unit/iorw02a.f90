! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iorw.presh iorw02a
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
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN, READ and WRITE
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access,
!*                              : read it at default position (1)
!*                              : without POS specifier, and
!*                              : rewrite it.
!234567890123456789012345678901234567890123456789012345678901234567890
program iorw02a
  character(4) c
  integer(4) :: I = 5, J = 2, K

  open(1, file='mystream', access='stream', form='formatted', &
       action='readwrite')

  ! Attempt to advancing character input starting
  ! at file position 2.

  read(1, 10) c
  print *, c

  write(1, 20) I
  inquire(1, pos=K)
  print *, K
  close(1)

10    FORMAT (A4)
20    FORMAT (I4)
end program iorw02a

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1234abc\n   5\n" after the
  ! program is done. Because by default, advance='yes'.
