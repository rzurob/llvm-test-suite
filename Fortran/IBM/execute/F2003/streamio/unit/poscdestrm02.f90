! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The usage of the control edit
!*                             : descriptor ':' with stream formatted
!*                             : access.
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      Program poscdestrm02

      integer iostat, pos, int1, int2, int3
      real r1, r2

      open(11, status='scratch', access='stream', form='formatted', iostat=iostat)

      if (iostat <> 0) error stop 1
      write(11, fmt='(I8)', iostat=iostat) 12345678
      if (iostat <> 0) error stop 2

      rewind 11
      read(11, fmt='(:2I3, I2)', advance='no', iostat=iostat) int1, int2

      if (iostat <> 0) error stop 3
      if (int1 <> 123) error stop 4
      if (int2 <> 456) error stop 5

      inquire(11, pos=pos)
      if (pos <> 7) error stop 6

      read(11, fmt='(I1)', iostat=iostat) int3
      if (iostat <> 0) error stop 7
      if (int3 <> 7) error stop 8

      inquire(11, pos=pos)
      if (pos <> 10) error stop 9

      backspace 11
      inquire(11, pos=pos)
      if (pos <> 1) error stop 10

      End Program poscdestrm02
