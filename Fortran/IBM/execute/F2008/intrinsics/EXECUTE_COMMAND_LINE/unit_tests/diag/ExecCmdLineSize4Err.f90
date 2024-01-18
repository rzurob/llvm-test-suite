!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=4
! %GROUP: ExecCmdLineSize2Err.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      program ExecCmdLineSize4Err
        logical*8 :: wait_arg = .TRUE.
        call execute_command_line("ls", wait_arg)
      end
