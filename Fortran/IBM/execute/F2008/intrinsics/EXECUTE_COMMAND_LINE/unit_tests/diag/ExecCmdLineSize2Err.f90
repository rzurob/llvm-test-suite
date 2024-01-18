!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=2
! %GROUP: ExecCmdLineSize2Err.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
      program ExecCmdLineSize2Err
        logical*4 :: wait_arg = .TRUE.
        call execute_command_line("ls", wait_arg)
      end
