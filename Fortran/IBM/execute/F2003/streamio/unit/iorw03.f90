! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iorw.presh iorw03
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
!* TEST CASE TITLE              : iorw03.f
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
!*                              : and 'append' postion, read it at 
!*				: default position (1) without POS 
!*				: specifier, and rewrite it.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program iorw03
  character(4) c
  integer(4) :: I = 5, J = 2, K

  open(1, file='mystream', access='stream', form='formatted', &
       action='readwrite', position='append')

  ! Write positions 8-11 (inclusive) with the value
  ! of the 4-byte integer variable I.
  write(1, 20, advance='no') I
  inquire(1, pos=K)
  print *, K
  close(1)

20    FORMAT (I4)
end program iorw03

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1234abc\n   5" after the
  ! program is done.
