!************************************************************************
      program ExecCmdLineSize2Err
        logical*4 :: wait_arg = .TRUE.
        call execute_command_line("ls", wait_arg)
      end
