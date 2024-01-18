! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export XLFRTEOPTS=multconn=yes
! %COMPOPTS: -qfree=f90
! %GROUP: seqstrm.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f ./seqfor.data
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : A formatted file "seqfor.data" is
!*                             : created and connected to two units for
!*                             : both SEQUENTIAL and STREAM access.
!*                             : INQUIRE statement.
!*                             : POSITION specifier in the OPEN statement.
!*                             : FLUSH statement.
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program seqstrm

      integer seqitem, strmitem
      integer I, J, iostat, pos, size
      character*10 access, form, strm

      open(11, file='seqfor.data', access='sequential', form='formatted', iostat=iostat)
      if (iostat <> 0) error stop 1
      J = 10
      do I = 1, 5
         write(11, *, iostat=iostat) I * J
         J = J * 10
      end do
      call flush_(11)

      open(12, file='seqfor.data', access='stream', form='formatted', position='append', iostat=iostat)
      if (iostat <> 0) error stop 2

      inquire(12, pos=pos, size=size)
      if (pos <> 31) error stop 3
      if (size <> 30) error stop 4

      rewind(11)
      read(11, *, iostat=iostat) seqitem
      if (iostat <> 0) error stop 5

      rewind 12
      do 20 I =1, 2
         read(12, *, iostat=iostat) strmitem
         if (iostat <> 0) error stop 6
20    continue

      inquire(12, pos=pos)
      if (pos <> 10) error stop 7

      read(11, *, iostat=iostat) seqitem
      if (iostat <> 0) error stop 8
      if (seqitem <> 200) error stop 9

      read(12, *, pos=16, iostat=iostat) strmitem
      if (strmitem <> 40000) error stop 10

      backspace 12
      read(12, *, iostat=iostat) strmitem
      if (iostat <> 0) error stop 11
      if (strmitem <> 40000) error stop 12

      inquire(12, access=access, form=form, stream=strm)
      if (access <> 'STREAM') error stop 13
      if (form <> 'FORMATTED') error stop 14
      if (strm <> 'YES') error stop 15

      End Program seqstrm
