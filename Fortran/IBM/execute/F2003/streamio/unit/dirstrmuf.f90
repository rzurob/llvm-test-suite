! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : An unformatted file is created and
!*                             : connected for both Direct and Stream
!*                             : access.
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program dirstrmuf

      integer diritem, strmitem, I, iostat

      open(11, file='diruf', access='direct', status='new', recl=4, action='readwrite', iostat=iostat)
      if (iostat <> 0) error stop 1
      do 20 I =1, 9
         write(11, rec=I, iostat=iostat) I
         if (iostat <> 0) error stop 2
20    continue
      call flush_(11)

      open(12, file='diruf', access='stream', iostat=iostat)
      if (iostat <> 0) error stop 3

      read(11, rec=5, iostat=iostat) diritem
      if (iostat <> 0) error stop 4
      if (diritem .ne. 5) error stop 5

      read(12, pos=5, iostat=iostat) strmitem
      if (iostat <> 0) error stop 6
      if (strmitem <> 2) error stop 7

      read(12, iostat=iostat) strmitem
      if (iostat <> 0) error stop 8
      if (strmitem .ne. 3) error stop 9

      End Program dirstrmuf
