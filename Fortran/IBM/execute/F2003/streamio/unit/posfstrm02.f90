! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : POS change with formatted
!*                             : stream access when Pad is set to no.
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      Program posfstrm02

      integer iostat, pos, size
      character*2 c1, c2
      character*4 ch1, ch2

      open(11, status='scratch', access='stream', form='formatted', pad='no', iostat=iostat)
      if (iostat <> 0) error stop 1

      write(11, '(A2/A2)', iostat=iostat) 'ab', 'cd'
      if (iostat <> 0) error stop 2

      inquire(11, pos=i)
      if (i <> 7) error stop 3

      read(11, *, iostat=iostat) ch1
      if (iostat <> -1) error stop 4


      read(11, '(A4)', pos=4, advance='no', iostat=iostat) ch1
      if (iostat <> -1) error stop 5
      inquire(11, pos=pos)
      if (pos <> 7) error stop 6

      read(11, '(A4)', pos=1, iostat=iostat) ch1
      if (iostat <> 4) error stop 7
      inquire(11, pos=pos)
      if (pos <> 4) error stop 8

      write(11, '($, A4)', pos=7, iostat=iostat) 'efgh'
      if (iostat <> 0) error stop 9
      inquire(11, pos=pos, size=size)
      if (pos <> 11) error stop 10
      if (size <> 10) error stop 11

      read(11, '(A4)', iostat=iostat) c1
      if (iostat <> -1) error stop 12
      inquire(11, pos=pos)
      if (pos <> 11) error stop 13

      read(11, '(A4)', advance='no', iostat=iostat) c1
      if (iostat <> -1) error stop 14

      read(11, '(A4)', pos=9, iostat=iostat) ch1
      if (iostat <> -1) error stop 15

      inquire(11, pos=pos, size=size)
      if (pos <> 11) error stop 16
      if (size <> 10) error stop 17

      read(11, '(A4)', pos=9, advance='no', iostat=iostat) ch1
      if (iostat <> -1) error stop 18
      inquire(11, pos=pos, size=size)

      End Program posfstrm02
