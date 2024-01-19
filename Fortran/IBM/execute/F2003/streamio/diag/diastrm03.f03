! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The POS specifier can only appear in
!*                             : input/output statements that specify a
!*                             : unit connected for stream access.
!*                             : The POS specifier must not be used on
!*                             : data transfer statements with files
!*                             : that cannot be positioned.
!*                             : Recovering from recoverable error
!*                             : conditions is also tested.
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      Program diastrm03

      integer iostat, number, i
      character*2 c

!     Opening a sequential formatted file unit by default
      open(11, status='scratch', iostat=iostat)
      if (iostat <> 0) error stop 1

      write(11, fmt='(I3)', pos=1, iostat=iostat) 111
      if (iostat <> 196) error stop 2

!     recovering tests
      write(11, fmt='(I2)', iostat=iostat) 22
      if (iostat <> 0) error stop 3

      read(11, fmt='(I2)', pos=1, iostat=iostat) number
      if (iostat <> 196) error stop 4

!     recovering tests
      rewind(11)
      read(11, fmt='(I2)', iostat=iostat) number
      if (iostat <> 0) error stop 5
      if (number <> 22) error stop 6

      open(12, status='scratch', access='direct', recl=2, iostat=iostat)
      if (iostat <> 0) error stop 7

      write(12, pos=1, iostat=iostat) "ab"
      if (iostat <> 196) error stop 8

!     recovering tests
      write(12, rec=1, iostat=iostat) "cd"
      if (iostat <> 0) error stop 9

      read(12, pos=1, iostat=iostat) c
      if (iostat <> 196) error stop 10

!     recovering tests
      read(12, rec=1, iostat=iostat) c
      if (iostat <> 0) error stop 11
      if (c <> 'cd') error stop 12

      read(13, pos=1, iostat=iostat) c
      if (iostat <> 198) error stop 13

      End Program diastrm03
