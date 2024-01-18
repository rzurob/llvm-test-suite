! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iorw.presh iorw01c
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
!*                              : read it at specific position which
!*                              : is pointed by POS specifier, and
!*                              : rewrite it. It is unformatted file.
!234567890123456789012345678901234567890123456789012345678901234567890
program iorw01c
  character(4) c
  integer(2) :: I = 12337, J = 2
  integer K

  open(1, file='mystream', access='stream', form='unformatted', &
       action='readwrite')

  ! Attempt to do advancing character input starting
  ! at file position 2.

  read(1, POS=J) c
  print *, c

  ! Overwrite positions 2-3
  write(1, POS=J) I
  inquire(1, pos=K)
  print *, K
  close(1)

end program iorw01c

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1014abc\n" after the
  ! program is done.
