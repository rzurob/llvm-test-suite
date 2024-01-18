! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/iorw.presh iorw09
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
!*                              : rewrite it. All above happened in
!*                              : module.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer(4) :: I = 5, J = 2, K
character(4) c

contains
subroutine sub
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

end subroutine
end module

program iorw09
  use mod
  call sub
end program iorw09

  ! Assuming mystream contained "1234abc\n" before the
  ! program was run, it would contain "1   5\nc\n" after the
  ! program is done.
