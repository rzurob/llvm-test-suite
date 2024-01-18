! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iorw.presh iorw01b
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
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : iorw01b.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Mar. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : stream I/O: OPEN, READ and WRITE
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : open a file by 'stream' access,
!*                              : read it at specific position which
!*                              : is pointed by POS specifier, and
!*                              : rewrite it.
!*                              : 
!234567890123456789012345678901234567890123456789012345678901234567890
program iorw01b
  character(4) c
  integer(4) :: I = 5, J = 2, K

  open(1, file='mystream', access='stream', form='formatted', &
       action='readwrite')

  ! Attempt to do advancing character input starting
  ! at file position 2.

  read(1, FMT='(A4)', POS=J) c
  print *, c

  write(1, FMT='(I4)', POS=J) I
  inquire(1, pos=K)
  print *, K
  close(1)

end program iorw01b

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1   5\nc\n" after the
  ! program is done. By default, read/write with ADVANCE='YES'.
