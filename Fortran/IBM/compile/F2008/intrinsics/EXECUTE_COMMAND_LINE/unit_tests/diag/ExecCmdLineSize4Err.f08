!************************************************************************
      program ExecCmdLineSize4Err
        logical*8 :: wait_arg = .TRUE.
        call execute_command_line("ls", wait_arg)
      end
