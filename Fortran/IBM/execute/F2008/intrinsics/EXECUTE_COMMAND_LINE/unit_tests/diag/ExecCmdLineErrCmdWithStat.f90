!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ExecCmdLineErrCmdWithStat.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      program ExecCmdLineErrCmdWithStat
        integer :: e
        integer :: c
        character(90) :: msg = "No error!"

        print *, trim(msg)

        call execute_command_line("cat no", exitstat=e, cmdstat=c, &
          cmdmsg = msg)

        print *, e
        print *, c
        print *, trim(msg)
      end
