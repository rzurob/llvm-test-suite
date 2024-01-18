!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=8
! %GROUP: ExecCmdLineSize8Err.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      program ExecCmdLineSize8Err
        logical*4 :: wait_arg = .TRUE.
        call execute_command_line("ls", wait_arg)
      end
